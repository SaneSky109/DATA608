# -*- coding: utf-8 -*-
"""
Created on Sat Oct 22 11:58:06 2022

@author: ericl
"""

import pandas as pd
import numpy as np
import dash
from dash import Dash, html, dcc
import plotly.graph_objs as go
import plotly.express as px

# Q1 Data

def get_tree_health(borough):
    url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,health&' +\
        '$where=boroname=\'' + borough +\
        '\'&$limit=683788').replace(' ', '%20')
    trees = pd.read_json(url)
    trees = trees.dropna()    
    
    trees = pd.crosstab(trees['spc_common'], trees['health'], margins=True)
    trees["Borough"] = borough
    trees["Type"] = trees.index
    
    return trees

boroughs = ['Queens', 'Brooklyn', 'Manhattan', 'Staten Island', 'Bronx']

Queens = get_tree_health(boroughs[0])
Brooklyn = get_tree_health(boroughs[1])
Manhattan = get_tree_health(boroughs[2])
StatenIsland = get_tree_health(boroughs[3])
Bronx = get_tree_health(boroughs[4])

master_data = pd.concat([Queens, Brooklyn, Manhattan, StatenIsland, Bronx])
master_data.reset_index(drop = True, inplace=True)

Borough = master_data['Borough'].tolist()
Type = master_data['Type'].tolist()
Fair = master_data['Fair'].tolist()
Good = master_data['Good'].tolist()
Poor = master_data['Poor'].tolist()
Total = master_data['All'].tolist()

dataset = {
    'Borough':Borough,
    'Type':Type,
    'Fair':Fair,
    'Good':Good,
    'Poor':Poor,
    'Total':Total
          }

df = pd.DataFrame(dataset)

df['FairPercent'] = df['Fair'] / df['Total']
df['GoodPercent'] = df['Good'] / df['Total']
df['PoorPercent'] = df['Poor'] / df['Total']


# Q2 Data

def get_steward_data(borough):
    url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,steward,health&' +\
        '$where=boroname=\'' + borough +\
        '\'&$limit=683788').replace(' ', '%20')
    trees = pd.read_json(url)
    trees = trees.dropna()    
    
    trees = pd.crosstab(trees['spc_common'], [trees['health'], trees['steward']], margins=True)
    trees["Borough"] = borough
    trees["Type"] = trees.index
    
    return trees

boroughs = ['Queens', 'Brooklyn', 'Manhattan', 'Staten Island', 'Bronx']

Queens2 = get_steward_data(boroughs[0])
Brooklyn2 = get_steward_data(boroughs[1])
Manhattan2 = get_steward_data(boroughs[2])
StatenIsland2 = get_steward_data(boroughs[3])
Bronx2 = get_steward_data(boroughs[4])

master_data2 = pd.concat([Queens2, Brooklyn2, Manhattan2, StatenIsland2, Bronx2])
master_data2.reset_index(drop = True, inplace=True)


variables = master_data2.columns.tolist()
tree_type = master_data2['Type']

tree_type = pd.concat([tree_type, tree_type, tree_type,
                      tree_type, tree_type, tree_type,
                      tree_type, tree_type, tree_type,
                      tree_type, tree_type, tree_type])

location = []
for k in range(12):
    #print(k)
    for i in range(5):
       # print(i)
        for j in range(132):
            if(i == 0):
                location.append("Queens")
            if(i == 1):
                location.append("Brooklyn")
            if(i == 2):
                location.append("Manhattan")
            if(i == 3):
                location.append("StatenIsland")
            if(i == 4):
                location.append("Bronx")
                
df2_clean = pd.melt(master_data2, value_vars=variables)
df2_clean =df2_clean[:7920]


health2 = df2_clean['health'].tolist()
steward2 = df2_clean['steward'].tolist()
value2 = df2_clean['value'].tolist()
Type2 = tree_type
Borough2 = location

dataset2 = {
    'health':health2,
    'steward':steward2,
    'value':value2,
    'Type':Type2,
    'Borough':Borough2
          }

df2_cleaned = pd.DataFrame(dataset2)



# Dash App

app = Dash()

app.layout = html.Div([
    html.H1('DATA608 Module 4 Assignment'),
    html.H2('Eric Lehmphul'),
    
     html.Div([
        html.Label('Select Borough'),
        dcc.Dropdown(
            id='borough',
            options=[{'label': i, 'value': i} for i in boroughs],
            value='Manhattan')
     ]),
    html.Div([
        dcc.Graph(id='graph1')
    ]),
    html.Div([
        dcc.Graph(id='graph2'),
        html.Label('Select Tree Type'),
        dcc.Dropdown(
            id='tree_type',
            options=[{'label': i, 'value': i} for i in Type2],
            value='American beech')
    ]),
])


@app.callback(
    dash.dependencies.Output('graph1', 'figure'),
    dash.dependencies.Input('borough', 'value'))
def update_graph(borough):
    data = df[df['Borough'] == str(borough)] 

    fig = go.Figure(go.Bar(x=data['Type'],
        y=data['GoodPercent'],
        name='Good'))

    fig.add_trace(go.Bar(x=data['Type'],
        y=data['FairPercent'],
        name='Fair'))
    fig.add_trace(go.Bar(x=data['Type'],
        y=data['PoorPercent'],
        name='Poor'))

    fig.update_layout(title=("Question 1: Tree Health of " + str(borough)), barmode='stack', yaxis=dict(autorange="reversed"))
    return fig
        


@app.callback(
    dash.dependencies.Output('graph2', 'figure'),
    [dash.dependencies.Input('borough', 'value'),
     dash.dependencies.Input('tree_type', 'value')])
def update_graph2(borough, tree_type):
    data = df2_cleaned[(df2_cleaned['Borough'] == str(borough)) & (df2_cleaned['Type'] == str(tree_type))]

    fig = px.histogram(data, x="steward",
                   y="value", color="health",
                   barnorm='percent', text_auto='.2f',
                   title="Question 2: Tree Health of " + str(tree_type) + " in " + str(borough) + " by Steward Number")
    
    return fig




if __name__ == '__main__':
    app.run_server(debug=True)
        


