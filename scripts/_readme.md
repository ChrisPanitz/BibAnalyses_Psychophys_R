# subfolder scripts
Contains R scripts for analyses and plotting

- 01_aggregateData: Read txt files of WoS output, create bibliometrix data frame and save it
- 02_summaryAnalyses: Run standard bibliometrix descriptive analyses on dataset
- 03_journalPerformance: Run analyses & create plots on journal performace (publications, submissions, IF)
- 04_authorTeams: Run analyses and plot results on authorship analyses (team size, gender of submitting authors, international co-authorships)
- 05_historicalCitations: Create and plot historical direct citation network on top locally cited papers
- 06_extractKeyTerms_title: Extract (multi- and single-word) key terms from publication titles and write them into txt file
- 07_titleTrends: List top 10 key terms for each decade & all time; draw top 10 all-time over the years; create tables & plots
- 08_export4vos: Compute co-occurrences of top XX key terms and export them for VOS Viewer
- 09_keyterm_coocurrenceClusters: Compute and plot centrality and density for VOS Viewer clusters, write tables describing clusters