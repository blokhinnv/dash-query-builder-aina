/**
 * Переводит конфиг UI на русский язык
 */
const translateConfig = (originalConfig) => {
    let newConfig = {...originalConfig};
    newConfig.settings = {
        ...newConfig.settings,
        fieldLabel: 'Поле',
        addRuleLabel: 'Добавить правило',
        addGroupLabel: 'Добавить группу',
        notLabel: 'Не',
        addCaseLabel: 'Добавить условие',
        addDefaultCaseLabel: 'Добавить условие по умолчанию',
        addSubRuleLabel: 'Добавить условие подправило',
        defaultCaseLabel: 'По умолчанию: ',
        fieldPlaceholder: 'Выберите поле',
        funcLabel: 'Функция',
        funcPlaceholder: 'Выберите функцию',
        operatorLabel: 'Оператор',
        operatorPlaceholder: 'Выберите оператор',
        valueSourcesPopupTitle: 'Выберите источник значения',
    };
    newConfig.conjunctions.AND.label = 'И';
    newConfig.conjunctions.OR.label = 'Или';
    let translation = {
        text: {
            valuePlaceholder: 'Введите строку',
            valueLabel: 'Строка',
        },
        number: {
            valuePlaceholder: 'Введите число',
            valueLabel: 'Число',
        },
        slider: {
            valuePlaceholder: 'Введите число или передвиньте слайдер',
            valueLabel: 'Число',
        },
        time: {
            valuePlaceholder: 'Введите время',
            valueLabel: 'Время',
        },
        textarea: {
            valuePlaceholder: 'Введите текст',
            valueLabel: 'Текст',
        },
        select: {
            valuePlaceholder: 'Выберите значение',
            valueLabel: 'Значение',
        },
        multiselect: {
            valuePlaceholder: 'Выберите значения',
            valueLabel: 'Значения',
        },
        func: {
            valuePlaceholder: 'Выберите функцию',
            valueLabel: 'Функция',
        },
        field: {
            valuePlaceholder: 'Выберите поле для сравнения',
            valueLabel: 'Поле для сравнения',
        },
        datetime: {
            valuePlaceholder: 'Выберите дату и время',
            valueLabel: 'Дата и время',
        },
        date: {
            valuePlaceholder: 'Выберите дату',
            valueLabel: 'Дата',
        },
        boolean: {
            labelNo: 'Нет',
            labelYes: 'Да',
        },
    };
    Object.entries(translation).forEach((entry) => {
        const [type, translatedFields] = entry;
        newConfig.widgets[type] = {
            ...newConfig.widgets[type],
            ...translatedFields,
        };
    });

    return newConfig;
};

export default translateConfig;
