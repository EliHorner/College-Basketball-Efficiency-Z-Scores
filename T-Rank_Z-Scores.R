library(data.table)
# http://barttorvik.com/2023_fffinal.csv
bdata <- fread('2023_fffinal.csv')
set(bdata, j = c(3L, 5L, 7L, 9L, 11L, 13L, 15L, 17L, 19L,
                 21L, 23L, 25L, 27L, 29L, 31L, 33L, 35L,
                 37L, 39L, 41L), value = NULL)

cols <- colnames(bdata)[2:21]
out_cols <- paste(colnames(bdata)[2:21], 'z', sep = "_")
bdata[, c(out_cols) := lapply(.SD, function(x){((x - mean(x))/sd(x))}), .SDcols = cols]

z_cols <- colnames(bdata)[22:41]
a_cols <- z_cols[grepl('D', z_cols, fixed = TRUE)]
bdata[, c(a_cols) := lapply(.SD, function(x){(x * -1)}), .SDcols = a_cols]

bdata$`TO%_z` <- -1 * bdata$`TO%_z`
bdata$`TO% Def._z` <- -1 * bdata$`TO% Def._z`


# http://barttorvik.com/2023_team_results.csv
b2data <- fread('2023_team_results.csv')
b2data[, adjoe_z := ((adjoe - mean(adjoe))/ sd(adjoe)),]
b2data[, adjde_z := -((adjde - mean(adjde))/ sd(adjde)),]

merge_cols <- c('rank', 'team', 'barthag', 'adjoe', 'adjde', 'adjoe_z', 'adjde_z')
b2sub <- b2data[, .SD, .SDcols = merge_cols]

bzdata <- merge(b2sub, bdata, by.x = 'team', by.y = 'TeamName', all = TRUE)
bzdata[order(rank)]
fwrite(bzdata[order(rank)], 'zScores030523.csv')