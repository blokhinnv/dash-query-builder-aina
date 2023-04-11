import React, {
    Component,
    useCallback,
    useContext,
    useEffect,
    useState,
} from 'react';
import {
    Utils as QbUtils,
    Query,
    Builder,
    BasicConfig,
} from '@react-awesome-query-builder/ui';
import translateConfig from '../translate/translate';
import {
    SettingsContext,
    TreeContext,
    getCurrentSettings,
} from '../contexts/SettingsContext';
import fieldSetsEqual from '../utils';

/* import {Utils, Query, Builder} from '@react-awesome-query-builder/ui';
const {
    loadTree,
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
    uuid,
} = Utils;*/

const emptyTree = {id: QbUtils.uuid(), type: 'group'};

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

const BaseQueryBuilder = (props) => {
    const {fields, alwaysShowActionButtons, tree} = props;
    const [config, setConfig] = useState({
        ...translateConfig(props.config),
        fields: fields,
    });
    const [state, setState] = useState({
        tree: QbUtils.checkTree(QbUtils.loadTree(tree || emptyTree), config),
    });
    const {updateProps} = useContext(SettingsContext);

    useEffect(() => {
        if (!fieldSetsEqual(props.fields, config.fields)) {
            const updatedConfig = {...config, fields: props.fields};
            setConfig(updatedConfig);
            setState((prevState) => ({
                ...prevState,
                tree: QbUtils.checkTree(state.tree, updatedConfig),
                config: updatedConfig,
            }));
        }
    }, [props.fields]);

    const onChange = useCallback((immutableTree, config) => {
        setState((prevState) => ({
            ...prevState,
            tree: immutableTree,
            config: config,
        }));

        updateProps(getCurrentSettings(immutableTree, config));
    }, []);

    const renderBuilder = useCallback((props) => {
        return (
            <div className="query-builder-container" style={{padding: '10px'}}>
                <div
                    className={
                        alwaysShowActionButtons
                            ? 'query-builder'
                            : 'query-builder qb-lite'
                    }
                >
                    <Builder {...props} />
                </div>
            </div>
        );
    }, []);

    return (
        <div>
            <Query
                {...config}
                value={state.tree}
                onChange={onChange}
                renderBuilder={renderBuilder}
            />
        </div>
    );
};

export default BaseQueryBuilder;
