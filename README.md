Introduction
=======================

Naturally, the relative cohesion of meaning in language is a product of mutual agreement in usage, that is
at least a useful working model. In this project we examine in particular how differences in language use is
related to geography. Of course, language use is tethered to where we live, that is, we communicate often
with people in physical proximity and are thus inclined to agree on meaning in word choice with those
around us. Given this relation, we play the role of computation dialectometrists, and ask: how are our
dialect differences reflected in geographic divisions?

Dialectromy, the study of dialect dierences, grew much from the simple (though eective) counting of
overlapping features technique of its inception. Feature selection, developing metrics for similarity through
dierent weight considerations, are among the techniques that dialectrometrists develop and rene, aided
by computational methods. In this paper, we focus on dimensionality reduction, coupled with various clus-
tering methods to study to what extent dialed dierences are dened by geographic regions. We test the
robustness of the ndings by using dierent cluster methods and metric penalties, cross validating against
dierent segments of the dataset, and varying aggregation transformations applied to the original dataset.

The Data
======================
The dataset for Lab 2 come from a linguistics survey created and conducted by Prof. Bert Vaux, associate
professor of linguistics at Harvard University and Scott Golder, graduate student in the Media lab at MIT.
The survey was administered online and completed by the 47471 participants. It consisted of 122 questions,
the rst 49 of which focused on phonetics, and the remaining (questions 50-122) on word/phrase choice.
The responses of these latter questions, recorded in the form of multiple choice of varying lengths, are the
observations analyzed in this paper. Faceted graphs of the responses for each question is provided on the
survey website maintained by Bert Vaux.

The raw data can be found in **data**, code in **data_analysis_code**, and detailed report of the data analysis in 
[writeup](writeup.pdf)

Conclusions 
======================
The linguistics survey dataset revealed a surprising amount of information about the geographic borders
of dialect dierences. It demonstrated the successful use of PCA to concentrate on a lower-dimensional
subspace of data where most of the variation in observations take place (and thus also helps ignore noise),
and robust cluster ndings using dierent penalties and aggregations of the dataset. Some room for further
investigation is to employ non-distance based clustering methods, and also to build predictive models based
on the learned clusters to further validate the clusters generated. The interpretability of the subspace given
by the PCA can also be examined in more detail to help weigh the more useful/polarizing questions in the
survey.

