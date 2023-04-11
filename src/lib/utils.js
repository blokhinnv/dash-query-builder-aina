const getFieldsSet = (cfg) => {
    let fields = new Set();
    Object.entries(cfg).forEach((entry) => {
        const [cat, catInfo] = entry;
        Object.keys(catInfo.subfields).forEach((key) => {
            fields.add(`${cat}.${key}`);
        });
    });
    return fields;
};

const fieldSetsEqual = (cfg1, cfg2) => {
    const fields1 = getFieldsSet(cfg1);
    const fields2 = getFieldsSet(cfg2);
    return (
        fields1.size === fields2.size &&
        [...fields1].every((x) => fields2.has(x))
    );
};

export default fieldSetsEqual;
