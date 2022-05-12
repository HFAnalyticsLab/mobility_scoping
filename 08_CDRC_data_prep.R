#save script in bucket 
## save this script to that folder in the bucket
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/' ## my bucket name

put_object(
  file = '08_CRDC_data_prep.R',
  object = '08_CDRC_data_prep.R',
  bucket = buck
)
