import { createContext } from "react";
import {
    Utils as QbUtils,
} from '@react-awesome-query-builder/ui';

export const getCurrentSettings = (immutableTree, config) => {
    let currentSettings = {
        tree: QbUtils.getTree(immutableTree),
        sqlFormat: QbUtils.sqlFormat(immutableTree, config),
        fields: config.fields
    };
    return currentSettings;
};

export const SettingsContext = createContext(null)
export const TreeContext = createContext(null)
