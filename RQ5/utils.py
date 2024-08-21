from sklearn.metrics import accuracy_score, precision_score, recall_score, roc_curve, auc
import pandas as pd
import numpy as np
import pickle
from sklearn import preprocessing, svm, tree, utils
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVR
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier, GradientBoostingClassifier
from sklearn.model_selection import StratifiedShuffleSplit, TimeSeriesSplit
from sklearn.neural_network import MLPClassifier
import lightgbm as lgb
from collections import Counter

from imblearn.over_sampling import SMOTE
sm = SMOTE(random_state=100)

scenarios = [['activemq-5.0.0', 'activemq-5.1.0', 'activemq-5.2.0'],
            ['activemq-5.1.0', 'activemq-5.2.0', 'activemq-5.3.0'],
            ['activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0'],
            ['camel-1.4.0', 'camel-2.9.0', 'camel-2.10.0'],
            ['camel-2.9.0','camel-2.10.0', 'camel-2.11.0'],
            ['derby-10.2.1.6', 'derby-10.3.1.4', 'derby-10.5.1.1'],
            ['groovy-1_5_7', 'groovy-1_6_BETA_1', 'groovy-1_6_BETA_2'],
            ['hbase-0.94.0', 'hbase-0.95.0', 'hbase-0.95.2'],
            ['hive-0.10.0', 'hive-0.12.0', 'hive-0.9.0'],
            ['jruby-1.1', 'jruby-1.4.0', 'jruby-1.5.0'],
            ['jruby-1.4.0', 'jruby-1.5.0', 'jruby-1.7.0.preview1'],
            ['lucene-2.3.0', 'lucene-2.9.0', 'lucene-3.0.0'],
            ['lucene-2.9.0', 'lucene-3.0.0', 'lucene-3.1'],
            ['wicket-1.3.0-beta1', 'wicket-1.3.0-beta2', 'wicket-1.5.3']
        ]
features_names = [
        'Added_lines', 'Del_lines', 'ADEV', 'OWN_LINE', 'COMM', 'MINOR_LINE', 'CountPath_Mean', 
        'DDEV', 'CountLine','MaxNesting_Mean','CountClassCoupled','CountStmtDecl',
        'SumCyclomaticStrict']

        # 'OWN_COMMIT', 'MINOR_COMMIT'
    #      , , , 
    #     'CountDeclMethodPrivate', 'AvgLineCode', 
    #     'MaxCyclomatic', 'CountDeclMethodDefault', 'AvgEssential',
    #     'CountDeclClassVariable',  'AvgCyclomatic',
    #     'AvgLine', 'CountDeclClassMethod', 'AvgLineComment',
    #     'AvgCyclomaticModified', 'CountDeclFunction', 'CountLineComment',
    #     'CountDeclClass', 'CountDeclMethod', 'SumCyclomaticModified',
    #     'CountLineCodeDecl', 'CountDeclMethodProtected',
    #     'CountDeclInstanceVariable', 'MaxCyclomaticStrict',
    #     'CountDeclMethodPublic', 'CountLineCodeExe', 'SumCyclomatic',
    #     'SumEssential',  'CountLineCode', 'CountStmtExe',
    #     'RatioCommentToCode', 'CountLineBlank', 'CountStmt',
    #     'MaxCyclomaticModified', 'CountSemicolon', 'AvgLineBlank',
    #     'CountDeclInstanceMethod', 'AvgCyclomaticStrict',
    #     'PercentLackOfCohesion', 'MaxInheritanceTree', 'CountClassDerived',
    #      'CountClassBase', 'CountInput_Max',
    #     'CountInput_Mean', 'CountInput_Min', 'CountOutput_Max',
    #     'CountOutput_Mean', 'CountOutput_Min', 'CountPath_Max',
    #      'CountPath_Min', 'MaxNesting_Max', 
    #     'MaxNesting_Min'
    # ]

domain_knowledge = {
    # high values, more buggy
    'positive' : ['COMM', 'Added_lines', 'OWN_LINE', 'MINOR_COMMIT', 'MINOR_LINE',  'ADEV', 'DDEV', 'Del_lines',
        'CountDeclMethodPrivate', 'AvgLineCode', 'CountLine', 'MaxCyclomatic', 

        'CountDeclMethodDefault', 'AvgEssential',
        'CountDeclClassVariable', 'SumCyclomaticStrict', 'AvgCyclomatic',
        'AvgLine', 'CountDeclClassMethod', 
        'AvgCyclomaticModified', 'CountDeclFunction', 
        'CountDeclClass', 'CountDeclMethod', 'SumCyclomaticModified',
        'CountLineCodeDecl', 'CountDeclMethodProtected',
        'CountDeclInstanceVariable', 'MaxCyclomaticStrict',
        'CountDeclMethodPublic', 'CountLineCodeExe', 'SumCyclomatic',
        'SumEssential', 'CountStmtDecl', 'CountLineCode', 'CountStmtExe',
        'CountStmt',
        'MaxCyclomaticModified', 'CountSemicolon', 'AvgLineBlank',
        'CountDeclInstanceMethod', 'AvgCyclomaticStrict',
        'PercentLackOfCohesion', 'MaxInheritanceTree', 'CountClassDerived',
        'CountClassCoupled', 'CountClassBase', 'CountInput_Max',
        'CountInput_Mean', 'CountInput_Min', 'CountOutput_Max',
        'CountOutput_Mean', 'CountOutput_Min', 'CountPath_Max',
        'CountPath_Mean', 'CountPath_Min', 'MaxNesting_Max', 'MaxNesting_Mean',
        'MaxNesting_Min'],
    # low values, more buggy
    'negative' : ['Del_lines', 'AvgLineComment', 'CountLineComment', 'RatioCommentToCode', 'CountLineBlank']
}
domain_knowledge = pd.concat([ 
    pd.DataFrame({'feature': domain_knowledge.get('positive'), 'value': ['positive']*len(domain_knowledge.get('positive'))}),
    pd.DataFrame({'feature': domain_knowledge.get('negative'), 'value': ['positive']*len(domain_knowledge.get('negative'))})
])
# domain_knowledge.head()



def evaluation_metrics(y_true, y_pred):
    fpr, tpr, thresholds = roc_curve(y_true=y_true, y_score=y_pred, pos_label=1)
    auc_ = auc(fpr, tpr)

    y_pred = [1 if p >= 0.5 else 0 for p in y_pred]

    acc = accuracy_score(y_true=y_true, y_pred=y_pred)
    prc = precision_score(y_true=y_true, y_pred=y_pred)
    rc = recall_score(y_true=y_true, y_pred=y_pred)
    f1 = 2 * prc * rc / (prc + rc)
    return acc, prc, rc, f1, auc_

def get_tptn(y_test, y_pred):
    df = pd.DataFrame(
        {"y_test" : y_test,
         "y_pred" : y_pred ,
         "prediction": [True if p >= 0.5 else False for p in y_pred] })

    top_tp = df.sort_values(by = 'y_pred', ascending=False)
    top_tp = top_tp[(top_tp['y_test'] ==True) & (top_tp['prediction'] ==True)]
    top_tp = top_tp.index[:20]


    top_tn = df.sort_values(by = 'y_pred')
    top_tn = top_tn[(top_tn['y_test'] ==False) & (top_tn['prediction'] ==False)]
    top_tn = top_tn.index[:20]

    misclassified = df[(df['y_test'] != df['prediction'])].index.values[:20]
    # df.iloc[top_tp].head()
    return top_tp, top_tn, misclassified


def select_top_features():
    df = None
    for pair in scenarios:
        i, j, k = pair

        i = 'datasets/' + i + '.csv'
        j = 'datasets/' + j + '.csv' 
        k = 'datasets/' + k + '.csv'

        X_train = pd.read_csv(i)[features_names]
        y_train = pd.read_csv(i)[['RealBug']].values.ravel()
        X_train, y_train = sm.fit_sample(X_train, y_train)

        X_test = pd.read_csv(j)[features_names]
        y_test = pd.read_csv(j)[['RealBug']].values.ravel()

        clf = RandomForestClassifier(random_state=100).fit(X_train, y_train)
        y_pred = clf.predict_proba(X_test)[:, 1]
        acc, prc, rc, f1, auc_ = evaluation_metrics(y_true=y_test, y_pred=y_pred)
        print('RF',acc, prc, rc, f1, auc_)

        importance = clf.feature_importances_ 

        feature_importances = pd.DataFrame(clf.feature_importances_,
                                           index = X_train.columns,
                                            columns=['importance'])
        df = pd.merge(df, feature_importances, left_index=True, right_index=True) if df is not None else feature_importances
        df['mean'] = df.mean(axis=1)
        return df.sort_values('mean', ascending=False).head(20)

def check_domain_knowldge(e, y_test):
    if len(e.split(' ')) == 3:
        left, condition, right = e.split(' ')
        if domain_knowledge.loc[domain_knowledge['feature'] == left, 'value'].values[0] == 'positive' and (condition in ['>','>=']):
            # print([idx,left,condition,right,X_test.loc[idx,left], "correct"])
            return [left,condition,right,True if y_test == True else False]
        else:
            # print([idx,left,condition,right,X_test.loc[idx,left], "incorrect"])
            return [left,condition,right,False if y_test == True else True]
    else:
        v1, c1, left, c2, v2 = e.split(' ')

        left, condition, right = left, c1, v1
        if c1 == '<': 
            condition = '>' 
        elif c1 == '<=':
            condition = '>='
        if domain_knowledge.loc[domain_knowledge['feature'] == left, 'value'].values[0] == 'positive' and (condition in ['>','>=']):
            # print([idx,left,condition,right,X_test.loc[idx,left], "correct"])
            return [left,condition,right,True if y_test == True else False]
        else:
            # print([idx,left,condition,right,X_test.loc[idx,left], "incorrect"])
            return [left,condition,right,False if y_test == True else True]