import React, {useCallback, useContext, useEffect, useState} from 'react';
import {
    Utils as QbUtils,
    Query,
    Builder,
} from '@react-awesome-query-builder/ui';
import translateConfig from '../translate/translate';
import {SettingsContext, getCurrentSettings} from '../contexts/SettingsContext';
import {fieldSetsEqual, switchRemoveIncomplete} from '../utils';

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

    const {fields, alwaysShowActionButtons} = props;
    const [config, setConfig] = useState({
        ...translateConfig(props.config),
        fields: fields,
    });
    const [tree, setTree] = useState(
        QbUtils.checkTree(QbUtils.loadTree(emptyTree), config)
    );
    const {updateProps} = useContext(SettingsContext);
    useEffect(() => {
        if (!fieldSetsEqual(props.fields, config.fields)) {
            const updatedConfig = switchRemoveIncomplete(
                {...config, fields: props.fields},
                true
            );
            setConfig(updatedConfig);
            const immutableTree = QbUtils.checkTree(
                QbUtils.loadTree(props.tree),
                updatedConfig
            );
            setTree(immutableTree);
            updateProps(getCurrentSettings(immutableTree, updatedConfig));
        }
    }, [props.fields]);
    useEffect(() => {
        if (props.tree === null) {
            return;
        }
        const immutableTree = QbUtils.checkTree(
            QbUtils.loadTree(props.tree),
            config
        );
        setTree(immutableTree);
    }, [props.tree]);
    const onChange = useCallback((immutableTree, config) => {
        setTree(immutableTree);
        if (config.settings.removeIncompleteRulesOnLoad) {
            const updatedConfig = switchRemoveIncomplete(config, false);
            setConfig(updatedConfig);
            updateProps(getCurrentSettings(immutableTree, updatedConfig));
            return;
        }
        setConfig(config);
        updateProps(getCurrentSettings(immutableTree, config));
    }, []);



    // Disabled React commponent
    const [pointerEv, setPointerEv] = useState('auto');
    const [styleDis, setStyleDis] = useState({});

    useEffect(() => {
        if(props.disabled){
            setPointerEv('none')
            setStyleDis({width: '100%', height: '100%', background: 'gray', opacity: "70%"});
        }else{
            setPointerEv('auto')
            setStyleDis({});
        }
    }, [props.disabled])




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
        <div style={styleDis}> 
            <div style={{pointerEvents: pointerEv}}>
                <Query
                    {...config}
                    value={tree}
                    onChange={onChange}
                    renderBuilder={renderBuilder}
                />
            </div>
        </div>
    );
};

export default BaseQueryBuilder;
