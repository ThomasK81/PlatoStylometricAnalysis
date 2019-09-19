## Plato Stylometric Analysis by Thomas Koentges

Data for the stylometric analysis of the _Corpus Platonicum_, with a special emphasis on the authorship of the _Menexenus_ submitted to [Greek, Roaman, and Byzantine Studies](https://grbs.library.duke.edu/index) on Sep 6, 2018 for peer review. The article is called "The Un-Platonic _Menexenus_: A Stylometric Analysis with More Data". The file 'TheUnplatonicMenexenus_PrePeerReviewVersion_Sept2018_ThomasKoentges.pdf' shows the pre-peer-review version. Since the first run of peer-review, in which both reviewers suggested to accept the contribution, but also gave some very helpful hints of how to further improve the article, all visualizations have changed. In particular, I replaced all CTS identifiers in the visualizations with accept abbreviations for authors and works and used 'Ward.D2' instead of 'Ward.D' for the implementation of the Ward-method in R. Several other points have been addressed and the improved article has been resubmitted to [Greek, Roaman, and Byzantine Studies](https://grbs.library.duke.edu/index) on Sep 19, 2019. None of the improvements changed anything about the initial finding that the _Menexenus_ is most likely not written by Plato. 

### What are the files in this repository

- The PDF is the article as it has been submitted to GRBS on Sep 6, 2018.
- The script `PlatoStyloResearch.R` contains the R-Script with which the results can be reproduced or expanded upon.
- `corpusParsed.csv.zip` is the corpus that has been extracted from [PerseusDL](http://opengreekandlatin.github.io/First1KGreek/) and the [First1KGreekProject](http://opengreekandlatin.github.io/First1KGreek/) using some little go programmes written for that purpose and not included in this repo, but some you can access [on one of my other repos](https://github.com/ThomasK81/TEItoCEX)
- `corpusplatonicum.csv` is the _Corpus Platonicum_ using logical CTS passages instead of just Stephanus pages. It has been generated automatically using R, Go, and thinking.
- `Weka4Gram.csv` and `WekaWord.csv` are an export of the features to be imported to WEKA.
- `Weka4GramFeaturesSeleted.csv` and `WekaWordFeaturesSeleted.csv` are the results of the attribute selection process using WEKA and described in the article.
- The folder `/tables/` contains all tables generated with my R script that were used for the analysis
- The folder`/plots/` contains all visualizations generated with my R script. Not all of them were used in the article and it contains very high-resolution images, which are detail-rich and would not make sense in a paper-based publication. Have a look at the different clusters and t-SNE visualizations. 

### Copyright
As soon as the article is accepted, I will also make this repository available on Zenodo and it will receive a DOI.
The copright is &copy;2017-2018 Thomas Koentges, but I use open licenses, so feel free to use it:
- MIT for the R-Script
- CC-BY for all images and text (unless otherwise specified)

