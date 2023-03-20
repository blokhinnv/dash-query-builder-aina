import React, { Component } from 'react';
import { Utils, Query, Builder } from '@react-awesome-query-builder/ui'
const { loadTree,
    _loadFromJsonLogic,
    loadFromSpel,
    checkTree,
    queryString,
    queryBuilderFormat,
    mongodbFormat,
    sqlFormat,
    jsonLogicFormat,
    elasticSearchFormat,
    spelFormat,
    getTree,
    uuid
} = Utils;
const emptyTree = { id: uuid(), type: 'group' };

// https://github.com/ukrbublik/react-awesome-query-builder/blob/35027ddc91e4211c9960e29c8e16ae851a425ec0/packages/ui/modules/components/Query.jsx
// https://github.com/ukrbublik/react-awesome-query-builder/blob/5af1548bcbc8001430b503aa4a1a15aa312a9af2/packages/examples/demo/index.tsx#L140-L146

/** DashQueryBuilder is a Dash Component based on [`react-awesome-query-builder`](https://github.com/ukrbublik/react-awesome-query-builder).
 *
 * It takes a `fields` property to generate the options for building the actual query.
 * The optional property `tree` is used to define the current state of the tree. It can be used to
 * define the starting state of the query builder.
 * The optional property `theme` is one of 'mui', `material`, `antd`, `bootstrap` or `basic`. This is the styling of the component.
 * These are the only themes supported by `react-awesome-query-builder`.
 */
export default class BaseQueryBuilder extends Component {
    constructor(props) {
        super(props);
        const fields = props.fields;
        const config = {
            ...props.config,
            fields,
        };
        // TODO: вынести выше
        config.settings = {
            ...config.settings,
            fieldLabel: "Поле",
            addRuleLabel: "Добавить правило",
            addGroupLabel: "Добавить группу",
            notLabel: "Не"
        }
        this.setProps = props.setProps;
        let loadFormat = (props.loadFormat === null || props.loadFormat === undefined) ? 'tree' : props.loadFormat;
        if (props.loadFormat === null || props.loadFormat === undefined) {
            this.setProps({ loadFormat: 'tree' })
        }
        let initialLoadItem = this.getLoadItem(loadFormat, props);
        let initialImmutableTree = checkTree(
            this.loadModifiedTree(props.loadFormat, initialLoadItem, config),
            config
        );

        this.state = {
            config: config,
            immutableTree: initialImmutableTree,
            alwaysShowActionButtons: props.alwaysShowActionButtons,
            loadFormat: props.loadFormat
        };
    }
    getLoadItem(loadFormat, props) {
        switch (loadFormat) {
            case 'jsonLogicFormat':
                return props.jsonLogicFormat
            case 'spelFormat':
                return props.spelFormat
            default:
            case 'tree':
                return props.tree
        }
    }
    loadModifiedTree(modifiedProp, modifiedValue, config = this.state.config) {
        switch (modifiedProp) {

            case 'jsonLogicFormat':
                if (modifiedValue === undefined || modifiedValue === null) {
                    return loadTree(emptyTree, config)
                }

                let treeAndErrors = _loadFromJsonLogic(modifiedValue, config);
                let tree = treeAndErrors[0];
                if (treeAndErrors[1].length > 0) {
                    console.log('There were errors loading the tree:', treeAndErrors[1]);
                    throw new Error('There were errors loading the tree: ' + treeAndErrors[1]);
                }
                return tree;

            case 'spelFormat':
                if (modifiedValue === '' || modifiedValue === undefined || modifiedValue === null) {
                    return loadTree(emptyTree, config)
                }
                else {
                    let treeAndErrors = loadFromSpel(modifiedValue, config);
                    let tree = treeAndErrors[0];
                    if (treeAndErrors[1].length > 0) {
                        console.log('There are Errors in the SPEL String', treeAndErrors[1]);
                        throw new Error('There were errors loading the tree: ' + treeAndErrors[1]);
                    }
                    if (tree === undefined) {
                        tree = loadTree(emptyTree, config);
                    }
                    return tree
                }

            case 'tree':
            default:
                return loadTree(modifiedValue || emptyTree, config);
        }

    }

    // getFields = (fields) => {
    //     let options = []
    //     Object.keys(fields).forEach((fieldName) => {
    //         let field = fields[fieldName]
    //         Object.keys(field).forEach((subfieldName) => {
    //             options.push(`${fieldName}.${subfieldName}`)
    //         })
    //     })
    //     return options
    // }

    // removeFields = (tree, fields) => {
    //     const { id, type, children1, properties } = tree;
    //     if (type === 'rule' && properties.field === fieldName) {
    //         return null;
    //     }

    //     if (children1 == null || children1 == undefined){
    //         return tree
    //     }

    //     const updatedChildren =
    //     children1
    //       .map((child) => this.removeFields(child, fieldName))
    //       .filter(Boolean);

    //     return {
    //       id,
    //       type,
    //       children1: updatedChildren,
    //       properties: { ...properties }
    //     };
    // };

    /**
     *
     * Update the state if tree has changed. This allows Dash to update the `tree` prop and have it set
     * the layout properly. Only run once and only if one of the props has changed.
     */
    componentDidUpdate(prevProps) {

        if (prevProps.fields !== this.props.fields) {
            let state = {...this.state}
            state.config.fields = this.props.fields

            let immutableTree = loadTree(emptyTree, state.config)
            // TODO: доработать, пока просто удаляем все
            // это приводит к ошибке
            // let immutableTree = loadTree(this.props.tree, state.config)
            // это почему-то обнуляет дерево?
            // immutableTree = checkTree(this.state.immutableTree, state.config)
            let currentState = this.getCurrentStateFromTree(immutableTree, state.config);

            this.setState({ immutableTree: immutableTree, config: state.config });
            this.setProps({...currentState, fields: this.props.fields});

        }
    }
    /**
     *
     *  Takes a tree and config and updates the various Formats used.
     */
    getCurrentStateFromTree = (immutableTree, config) => {
        let currentTree

        try {
            currentTree = getTree(immutableTree, true, false)
        }
        catch (e) {
            try {
                currentTree = getTree(immutableTree);
            }
            catch (e) {
                throw e
            }
        }
        let currentState = {
            tree: currentTree,
            config: config,
            queryStringFormat: queryString(immutableTree, config),
            queryBuilderFormat: queryBuilderFormat(immutableTree, config),
            mongodbFormat: mongodbFormat(immutableTree, config),
            sqlFormat: sqlFormat(immutableTree, config),
            jsonLogicFormat: jsonLogicFormat(immutableTree, config),
            elasticSearchFormat: elasticSearchFormat(immutableTree, config),
            spelFormat: spelFormat(immutableTree, config),
            loadFormat: this.state.loadFormat
        };
        return currentState;
    };

    findTree = (immutableTree) => {
        let jsonTree = getTree(immutableTree, true, true)
        if (jsonTree.children1 != undefined && jsonTree.children1.length == 0){
            return this.state.immutableTree
        }
        return immutableTree
    }

    onChange = (immutableTree, config, la) => {
        immutableTree = this.findTree(immutableTree)
        let currentState = this.getCurrentStateFromTree(immutableTree, config);
        this.setState({ immutableTree: immutableTree, config: config });
        this.setProps(currentState);
    };

    render = () => {
        return (
            <div>
                <Query
                    {...this.state.config}
                    value={this.state.immutableTree || emptyTree}
                    onChange={(immutableTree, config) => {this.onChange(immutableTree, config)}}
                    renderBuilder={this.renderBuilder}
                />
            </div>
        );
    };

    renderBuilder = (props) => {
        let tree = this.findTree(props.tree)
        props = {...props, tree: tree}
        return (<div className="query-builder-container" style={{ padding: '10px' }}>
            <div className={this.state.alwaysShowActionButtons ? 'query-builder' : 'query-builder qb-lite'}>
                <Builder {...props} />
            </div>
        </div>)
    }
}
