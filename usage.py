import json

from dash import Input, Output, State, callback_context, dash, html, no_update

import dash_query_builder

app = dash.Dash(__name__)
fields = {
    "fields": {
        "main_report_data": {
            "type": "!struct",
            "label": "Fields",
            "subfields": {
                "qty": {
                    "label": "Qty",
                    "type": "number",
                    "fieldSettings": {"min": 0},
                    "valueSources": ["value"],
                    "preferWidgets": ["number"],
                },
                "price": {
                    "label": "Price",
                    "type": "number",
                    "valueSources": ["value"],
                    "fieldSettings": {"min": 10, "max": 100},
                    "preferWidgets": ["slider", "rangeslider"],
                },
                "color": {
                    "label": "Color",
                    "type": "select",
                    "valueSources": ["value"],
                    "fieldSettings": {
                        "listValues": [
                            {"value": "yellow", "title": "Yellow"},
                            {"value": "green", "title": "Green"},
                            {"value": "orange", "title": "Orange"},
                        ]
                    },
                },
                "is_promotion": {
                    "label": "Promo?",
                    "type": "boolean",
                    "operators": ["equal", "is_empty"],
                    "valueSources": ["value"],
                },
            },
        }
    }
}
tree = {
    "id": "aa8abaa8-cdef-4012-b456-717f1e3484de",
    "type": "group",
    "manual": True,
    "children1": [
        {
            "id": "98a9baba-4567-489a-bcde-f17f1e35f74c",
            "type": "rule",
            "properties": {
                "field": "main_report_data.qty",
                "operator": "equal",
                "value": [3],
                "valueSrc": ["value"],
                "valueType": ["number"],
            },
        },
        {
            "id": "a99b99aa-0123-4456-b89a-b17f1e3608b4",
            "type": "rule",
            "properties": {
                "field": "main_report_data.price",
                "operator": "equal",
                "value": [11],
                "valueSrc": ["value"],
                "valueType": ["number"],
            },
        },
        {
            "id": "9a8b99a9-cdef-4012-b456-717f1e360a4c",
            "type": "rule",
            "properties": {
                "field": "main_report_data.color",
                "operator": "select_any_in",
                "value": [["green", "yellow"]],
                "valueSrc": ["value"],
                "valueType": ["multiselect"],
            },
        },
    ],
}
jsonLogicTree = {
    "errors": [],
    "logic": {
        "and": [
            {"==": [{"var": "main_report_data.qty"}, 3]},
            {"<=": [11, {"var": "main_report_data.price"}, 30]},
            {"in": [{"var": "main_report_data.color"}, ["green", "yellow"]]},
        ]
    },
    "data": {"main_report_data": {"qty": None, "price": None, "color": None}},
}
broken_jsonLogicTree = {
    "errors": [],
    "logic": {
        "and": [
            {"==": [{"var": "main_report_data.doesntExist"}, 3]},
        ]
    },
    "data": {"main_report_data": {"doesntExist": None}},
}

spelFormatTree = (
    "(main_report_data.qty == 3 && main_report_data.price >= 11 &&"
    + " main_report_data.price <= 30 &&"
    + " {'green', 'yellow'}.?[true].contains(main_report_data.color))"
)
broken_spelFormatTree = "(doesntExist == 3)"
empty_ = {"id": "889239a8-cdef-4012-b456-717b503a0ffb", "type": "group"}
app.layout = html.Div(
    [
        dash_query_builder.DashQueryBuilder(
            id="input",
            fields=fields["fields"],
            theme="bootstrap",
            alwaysShowActionButtons=True,
            tree=None,
            loadFormat="tree",
            disabled=False,
        ),
        html.Div(id="output"),
        html.Hr(),
        html.Button(id="update-button", children="Click to Update Using Tree Input"),
        html.Button(
            id="update-button-json", children="Click to Update Using JSONLogic Input"
        ),
        html.Button(
            id="update-button-spel", children="Click to Update Using SPEL Input"
        ),
        html.Hr(),
        html.Button(id="update-format-tree", children="Load Tree"),
        html.Button(id="update-format-json", children="Load JSONLogic"),
        html.Button(id="update-format-spel", children="Load SPEL"),
        html.Button(id="update-fields", children="update fields"),
        html.Button(id="btn-disabled", children="Disable buttons"),
    ]
)

@app.callback(
    Output("input", "tree"),
    Input("update-button", "n_clicks"),
    prevent_initial_call=True,
)
def update_tree_value(n):
    if n is not None and n % 2 == 1:
        rv = tree
    else:
        rv = empty_
    return rv

@app.callback(
    Output("input", "disabled"),
    Input("btn-disabled", "n_clicks"),
    # prevent_initial_call=True,
)
def btn_disabled(n):
    if n is not None and n % 2 == 1:
        return True
    return False



@app.callback(
    Output("input", "loadFormat"),
    [
        Input("update-format-tree", "n_clicks"),
        Input("update-format-json", "n_clicks"),
        Input("update-format-spel", "n_clicks"),
    ],
    prevent_initial_call=True,
)
def update_load_format(n_clicks_tree, n_clicks_json, n_clicks_spel):
    if not callback_context.triggered:
        return no_update
    else:
        button_id = callback_context.triggered[0]["prop_id"].split(".")[0]
        if button_id == "update-format-tree":
            return "tree"
        if button_id == "update-format-json":
            return "jsonLogicFormat"
        if button_id == "update-format-spel":
            return "spelFormat"


@app.callback(
    Output("output", "children"),
    [Input("input", "tree")],
    [
        State("input", "sqlFormat"),
        State("input", "jsonLogicFormat"),
        State("input", "spelFormat"),
        State("input", "fields"),
    ],
    prevent_initial_call=True,
)
def display_output(tree_value, sqlFormat, jsonLogicFormat, spelFormat, fields):
    output = html.Div(
        children=[
            html.H1("tree"),
            html.Div(json.dumps(tree_value, indent=2)),
            html.Hr(),
            html.H1("sqlFormat"),
            html.Div(sqlFormat),
            html.Hr(),
            html.H1("jsonLogicFormat"),
            html.Div(json.dumps(jsonLogicFormat)),
            html.Hr(),
            html.H1("spelFormat"),
            html.Div(spelFormat),
            html.Hr(),
            html.H1("fields"),
            html.Div(str(fields)),
        ]
    )
    return output


@app.callback(
    Output("input", "fields"),
    Input("update-fields", "n_clicks"),
    prevent_initial_call=True,
)
def update_tree_value(n):
    fs = {
        "main_report_data": {
            "type": "!struct",
            "label": "Fields",
            "subfields": {
                "qty": {
                    "label": "Qty",
                    "type": "number",
                    "fieldSettings": {"min": 0},
                    "valueSources": ["value"],
                    "preferWidgets": ["number"],
                },
            },
        }
    }
    print("upds", fs)
    return fs


@app.callback(
    Output("input", "jsonLogicFormat"),
    Input("update-button-json", "n_clicks"),
    prevent_initial_call=True,
)
def update_json_tree_value(n):
    if n is not None and n % 2 == 1:
        rv = jsonLogicTree
    else:
        rv = {}

    return rv


@app.callback(
    Output("input", "spelFormat"),
    Input("update-button-spel", "n_clicks"),
    prevent_initial_call=True,
)
def update_spel_tree_value(n):
    if n is not None and n % 2 == 1:
        rv = spelFormatTree
    else:
        rv = None

    return rv


if __name__ == "__main__":
    app.run_server(debug=True)