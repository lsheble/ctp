# load libraries 
library(rAltmetric)
library(magrittr)
library(purrr)
library(dplyr)


# read in the data
data_doi <- read.csv("/cloud/project/data/new-trainee-dois.txt", encoding="UTF-8", row.names=NULL, sep="", stringsAsFactors=FALSE)

# get the ids as a list
ids <- as.list(data_doi$doi)
# make sure it's a list
class(ids)

# get a list of data, catch errors (ie, ids that are not found. 
#                                   ids are printed to console)
altmetrics_lists = ids %>% map( ~ tryCatch( {altmetrics(doi = .x)},
                                             error = function(e) {
                                               cat(paste0("ID not found: ", .x, "\n"))
                                             }))

# fill the items with no data with Null values
altmetrics_lists[sapply(altmetrics_lists, is.null)] <- NULL

# make the list into a dataframe
altmetrics_df = altmetrics_lists %>% map_df(~rAltmetric::altmetric_data(.x))

# encoding to pass to write.table
encoding="UTF-8"

# instead of col numbers, using dplyr's select function since number of variables varies based on those included in the retrieved altmetric dataset
altmetrics_df.sel <- altmetrics_df %>% select(title, doi, pmid, uri, altmetric_id, score, history.at, pmc, cited_by_rdts_count, last_updated, added_on, published_on, journal, issns1, altmetric_jid, context.journal.count, context.all.higher_than, cited_by_tweeters_count, cited_by_fbwalls_count, cited_by_feeds_count, cited_by_accounts_count, cited_by_posts_count, cited_by_msm_count, readers.citeulike, readers.mendeley, readers.connotea, readers_count, starts_with("authors"), starts_with("subjects"))

# write the data to a flat "tsv" file delimited by pipes ("|")
write.table(altmetrics_df.sel, file = "/cloud/project/data/altmetrics_new-trainee-dois.tsv", fileEncoding=encoding, sep = "|", row.names=FALSE)
