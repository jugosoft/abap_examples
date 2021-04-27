@AbapCatalog.sqlViewName: 'ZV_CDV_DDIC_NAME' 
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Class i super'
define view ZV_CDV_DEMO_CDS
  as select from spfli
  association to scarr as _scarr on
    spfli.carrid = _scarr.carrid
  association to sflight as _sflight on
    spfli.connid = _sflight.connid
  {
    spfli.carrid                               as carrierId,
    concat(_scarr[inner].carrname, ' Carrier') as carrier,
    spfli.connid                               as flight,
    spfli.cityfrom                             as departure,
    spfli.cityto                               as arrival,
    case 
        when spfli.distance >= 100 
        then 'X' 
        else '' 
    end                                        as isLong, 
    spfli.distance                             as distance,                            
    division(distance, 3, 2)                   as inMiles,
    _scarr, // Make the associations visible
    _sflight
  } where _scarr.carrid = 'AA';