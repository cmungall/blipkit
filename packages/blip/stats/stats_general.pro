
% FP: type-I error
% FN: type-II error
% proporion of true negatives
% high specificity = low type I error rate
specificity(TN,FP,X):- X is TN / (TN+FP). % 
specificity(positive(TP,FP)-negative(TN,FN)):- X is TN / (TN+FP). % 

precision(TP,FP,X):- X is TP / (TP+FP). % positive predictive value (PPV)
precision(positive(TP,FP)-negative(TN,FN)):- X is TP / (TP+FP). % positive predictive value (PPV)

% propoprtion of true positives
sensitivity(TP,FN,X):- X is TP / (TP+FN). % aka recall
sesnitivity(positive(TP,FP)-negative(TN,FN)):- X is TP / (TP+FN). % aka recall
recall(TP,FN,X):- sensitivity(TP,FN,X).

negative_predictive_value(TN,TN,FN):- X is TN / (TN+FN).

f_measure(Precision,Recall,X):- X is 2 * ( (Precision / Recall) / (Precision + Recall) ).

