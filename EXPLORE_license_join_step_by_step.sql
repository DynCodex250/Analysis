-- =============================================================================
-- EXPLORE_license_join_step_by_step.sql
-- Zweck: Schritt-für-Schritt-Test des Joins zwischen
--        USERSECGOVRELATEDOBJECTS und den Licensing-Tabellen.
--
-- Anleitung:
--   Jeden Block (zwischen den === Trennlinien) einzeln markieren
--   und mit F5 / "Execute" ausführen.
--
-- Empfohlene Reihenfolge: Block 1 → 2 → 3 → 4 → 5 → 6 → 7
-- =============================================================================

-- Testparameter: Rolle zum Filtern (NULL = alle Rollen)
-- Zum Testen einer bestimmten Rolle hier den AOT-Namen eintragen:
DECLARE @TestRolle VARCHAR(200) = NULL
-- DECLARE @TestRolle VARCHAR(200) = 'RETAILTRIALADMINISTRATORROLE'


-- =============================================================================
-- BLOCK 1: Rohblick in USERSECGOVRELATEDOBJECTS
-- Frage: Wie sieht die Tabelle grundsätzlich aus?
-- =============================================================================

SELECT TOP 10
    ROLEIDENTIFIER,
    ROLENAME,
    DUTYIDENTIFIER,
    DUTYNAME,
    PRIVILEGEIDENTIFIER,
    PRIVILEGENAME,
    RESOURCE_,
    SECURABLETYPE,
    -- Die sechs Zugriffs-Bits:
    READACCESS,
    UPDATEACCESS,
    CREATEACCESS,
    DELETEACCESS,
    CORRECTACCESS,
    INVOKEACCESS
FROM USERSECGOVRELATEDOBJECTS
WHERE (@TestRolle IS NULL OR ROLEIDENTIFIER = @TestRolle)
ORDER BY ROLEIDENTIFIER, RESOURCE_


-- =============================================================================
-- BLOCK 2: Zugriffs-Bits → ACCESSLEVEL
-- Frage: Wie werden die 6 Bits zu einer einzigen Zahl (1 oder 2)?
-- Ziel:  1 = Lesen, 2 = Schreiben
-- =============================================================================

SELECT TOP 20
    ROLEIDENTIFIER,
    RESOURCE_,
    -- Original-Bits:
    READACCESS,
    UPDATEACCESS,
    CREATEACCESS,
    DELETEACCESS,
    CORRECTACCESS,
    INVOKEACCESS,
    -- Abgeleiteter ACCESSLEVEL:
    CASE
        WHEN UPDATEACCESS  = 1 OR CREATEACCESS  = 1
          OR DELETEACCESS  = 1 OR CORRECTACCESS = 1
          OR INVOKEACCESS  = 1 THEN 2          -- Schreibzugriff
        WHEN READACCESS    = 1               THEN 1          -- Lesezugriff
    END AS ACCESSLEVEL
FROM USERSECGOVRELATEDOBJECTS
WHERE (@TestRolle IS NULL OR ROLEIDENTIFIER = @TestRolle)
  AND (READACCESS + UPDATEACCESS + CREATEACCESS
     + DELETEACCESS + CORRECTACCESS + INVOKEACCESS) > 0
ORDER BY ROLEIDENTIFIER, RESOURCE_


-- =============================================================================
-- BLOCK 3: LicensingEntitlementObjects
-- Frage: Welche AOT-Objekte kennt Microsoft als lizenzpflichtig?
-- =============================================================================

SELECT TOP 20
    RECID,
    AOTNAME,       -- z.B. 'CustTableListPage', 'PurchCreateOrder'
    AOTCHILDNAME,
    SECURABLETYPE
FROM LicensingEntitlementObjects
ORDER BY AOTNAME


-- =============================================================================
-- BLOCK 4: Die vier Licensing-Tabellen zusammen (ObjLic)
-- Frage: Welche SKU braucht ein Objekt bei welchem Zugriffstyp?
-- =============================================================================

SELECT TOP 30
    obj.AOTNAME,
    laep.ACCESSLEVEL,       -- 1 = Lesen, 2 = Schreiben
    lsku.Priority,          -- 20 = Team Members, 60 = Finance, 80 = SCM, ...
    lsku.SKUNAME,           -- 'Finance', 'SCM', 'Team Members', ...
    lsku.GROUPNAME,
    lere.ENFORCEMENTPERMISSIONS
FROM LicensingEntitlementObjects AS obj
LEFT JOIN LicensingElementsRequiringEntitlement AS lere
       ON lere.ENTITLEMENTOBJECT = obj.RECID
LEFT JOIN LicensingAllEntitledPermissions AS laep
       ON laep.ENTITLEMENTOBJECT = obj.RECID
LEFT JOIN LicensingAllSkus AS lsku
       ON lsku.RECID = laep.SKURECID
WHERE laep.ACCESSLEVEL IS NOT NULL
ORDER BY obj.AOTNAME, laep.ACCESSLEVEL, lsku.Priority


-- =============================================================================
-- BLOCK 5: Vollständiger Join (SQL 16) – alle Rollen
-- Frage: Welche Lizenz braucht jede Rolle pro Objekt?
-- COALESCE: Wenn kein Eintrag in LicensingEntitlementObjects → 'Team Members'
-- =============================================================================

WITH UsgoBase AS (
    SELECT
        ROLEIDENTIFIER,
        ROLENAME,
        RESOURCE_,
        SECURABLETYPE,
        CASE
            WHEN UPDATEACCESS=1 OR CREATEACCESS=1
              OR DELETEACCESS=1 OR CORRECTACCESS=1
              OR INVOKEACCESS=1 THEN 2
            WHEN READACCESS=1   THEN 1
        END AS ACCESSLEVEL
    FROM USERSECGOVRELATEDOBJECTS
    WHERE (@TestRolle IS NULL OR ROLEIDENTIFIER = @TestRolle)
      AND (READACCESS + UPDATEACCESS + CREATEACCESS
         + DELETEACCESS + CORRECTACCESS + INVOKEACCESS) > 0
),
ObjLic AS (
    SELECT
        obj.AOTNAME,
        laep.ACCESSLEVEL,
        lsku.Priority,
        lsku.SKUNAME,
        lsku.GROUPNAME,
        lere.ENFORCEMENTPERMISSIONS
    FROM LicensingEntitlementObjects AS obj
    LEFT JOIN LicensingElementsRequiringEntitlement AS lere
           ON lere.ENTITLEMENTOBJECT = obj.RECID
    LEFT JOIN LicensingAllEntitledPermissions AS laep
           ON laep.ENTITLEMENTOBJECT = obj.RECID
    LEFT JOIN LicensingAllSkus AS lsku
           ON lsku.RECID = laep.SKURECID
    WHERE laep.ACCESSLEVEL IS NOT NULL
)
SELECT DISTINCT
    u.ROLEIDENTIFIER,
    u.ROLENAME,
    u.RESOURCE_                           AS AOTNAME,
    u.SECURABLETYPE,
    u.ACCESSLEVEL,
    -- Kein Eintrag in ObjLic → COALESCE liefert Fallback:
    COALESCE(o.Priority, 20)              AS Priority,
    COALESCE(o.SKUNAME, 'Team Members')   AS SKUNAME,
    o.GROUPNAME,
    o.ENFORCEMENTPERMISSIONS,
    -- Diagnose: wurde das Objekt in ObjLic gefunden?
    CASE WHEN o.AOTNAME IS NULL THEN 'Nicht in Licensing-Tabellen'
                                ELSE 'Gefunden'
    END AS LicensingStatus
FROM UsgoBase AS u
LEFT JOIN ObjLic AS o
       ON o.AOTNAME     = u.RESOURCE_
      AND o.ACCESSLEVEL = u.ACCESSLEVEL
ORDER BY
    u.ROLEIDENTIFIER ASC,
    Priority         DESC,
    u.RESOURCE_      ASC


-- =============================================================================
-- BLOCK 6: Was ist das teuerste Objekt pro Rolle?
-- Frage: Welche Basislizenz braucht jede Rolle mindestens?
-- Logik: MAX(Priority) über alle Objekte einer Rolle
-- =============================================================================

WITH UsgoBase AS (
    SELECT
        ROLEIDENTIFIER,
        ROLENAME,
        RESOURCE_,
        CASE
            WHEN UPDATEACCESS=1 OR CREATEACCESS=1
              OR DELETEACCESS=1 OR CORRECTACCESS=1
              OR INVOKEACCESS=1 THEN 2
            WHEN READACCESS=1   THEN 1
        END AS ACCESSLEVEL
    FROM USERSECGOVRELATEDOBJECTS
    WHERE (@TestRolle IS NULL OR ROLEIDENTIFIER = @TestRolle)
      AND (READACCESS + UPDATEACCESS + CREATEACCESS
         + DELETEACCESS + CORRECTACCESS + INVOKEACCESS) > 0
),
ObjLic AS (
    SELECT
        obj.AOTNAME,
        laep.ACCESSLEVEL,
        lsku.Priority,
        lsku.SKUNAME
    FROM LicensingEntitlementObjects AS obj
    LEFT JOIN LicensingAllEntitledPermissions AS laep
           ON laep.ENTITLEMENTOBJECT = obj.RECID
    LEFT JOIN LicensingAllSkus AS lsku
           ON lsku.RECID = laep.SKURECID
    WHERE laep.ACCESSLEVEL IS NOT NULL
),
RoleLicenses AS (
    SELECT DISTINCT
        u.ROLEIDENTIFIER,
        u.ROLENAME,
        COALESCE(o.Priority, 20) AS Priority,
        COALESCE(o.SKUNAME, 'Team Members') AS SKUNAME
    FROM UsgoBase u
    LEFT JOIN ObjLic o
           ON o.AOTNAME     = u.RESOURCE_
          AND o.ACCESSLEVEL = u.ACCESSLEVEL
)
SELECT
    ROLEIDENTIFIER,
    ROLENAME,
    -- Höchste Priorität = teuerste Lizenz = Mindestlizenz der Rolle
    MAX(Priority) AS MaxPriority,
    -- Den SKU-Namen zur höchsten Priorität ermitteln:
    MAX(CASE WHEN Priority = (
            SELECT MAX(Priority)
            FROM RoleLicenses r2
            WHERE r2.ROLEIDENTIFIER = r1.ROLEIDENTIFIER
        ) THEN SKUNAME END)      AS MinimumSKU,
    COUNT(DISTINCT SKUNAME)      AS AnzahlVerschiedenerSKUs,
    COUNT(*)                     AS AnzahlObjekte
FROM RoleLicenses r1
GROUP BY ROLEIDENTIFIER, ROLENAME
ORDER BY MaxPriority DESC, ROLEIDENTIFIER


-- =============================================================================
-- BLOCK 7: Objekte die NICHT in den Licensing-Tabellen stehen
-- Frage: Warum zeigt sec_struct "UNKNOWN" / kein Eintrag?
-- Diese Objekte fallen auf 'Team Members' zurück (COALESCE).
-- =============================================================================

WITH UsgoBase AS (
    SELECT DISTINCT
        RESOURCE_,
        SECURABLETYPE,
        CASE
            WHEN UPDATEACCESS=1 OR CREATEACCESS=1
              OR DELETEACCESS=1 OR CORRECTACCESS=1
              OR INVOKEACCESS=1 THEN 2
            WHEN READACCESS=1   THEN 1
        END AS ACCESSLEVEL
    FROM USERSECGOVRELATEDOBJECTS
    WHERE (@TestRolle IS NULL OR ROLEIDENTIFIER = @TestRolle)
      AND (READACCESS + UPDATEACCESS + CREATEACCESS
         + DELETEACCESS + CORRECTACCESS + INVOKEACCESS) > 0
)
SELECT
    u.RESOURCE_,
    u.SECURABLETYPE,
    u.ACCESSLEVEL,
    'Nicht in LicensingEntitlementObjects' AS Grund,
    'Team Members (Fallback)'              AS EffektiveSKU
FROM UsgoBase u
WHERE NOT EXISTS (
    SELECT 1
    FROM LicensingEntitlementObjects obj
    WHERE obj.AOTNAME = u.RESOURCE_
)
ORDER BY u.SECURABLETYPE, u.RESOURCE_
