@AbapCatalog.sqlViewName: 'YRETAB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cds For Relationship Table'
@Metadata.ignorePropagatedAnnotations: true
define view zrelationshiptab_CDS as select from zrelationshiptab
{
    key taskid as Taskid,
   key businesspartner as Businesspartner,
    description as Description,
    validfrom as Validfrom,
    validto as Validto,
    relationshipcatalog as Relationshipcatalog
}
