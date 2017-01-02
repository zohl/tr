import Inferno from 'inferno';
import {compose, modifyState, getJSON} from './common';
import spinner from './spinner';


const loadTranslation = (cname, query) => dname => (state, dispatch) => {
  var prevTrans = state.translations[cname][dname];
  var order = prevTrans ? prevTrans.order + 1 : 0;

  if (!query.length) {
    state.translations[cname][dname] = {
      loaded: true
    , order: order
    , entries: []
    };
    return;
  }

  state.translations[cname][dname] = {
    loaded: false
  , order: order
  };

  var cb = compose(dispatch, modifyState);

  getJSON(`/api/categories/${cname}/dictionaries/${dname}/${query}`, data => cb(state => {
    if (state.translations[cname][dname].order != order) {
      return;
    }

    state.translations[cname][dname].loaded = true;
    state.translations[cname][dname].entries = (data && data.length) ? data : [];
  }));
}


const loadTranslations = (cname, query) => (state, dispatch) => {
  if (undefined === state.translations) {
    state.translations = {};
  }

  if (undefined === state.translations[cname]) {
    state.translations[cname] = {};
  }

  state.dictionaries
    .filter(d => d.category == cname && d.loaded && d.enabled)
    .forEach(compose(dispatch, loadTranslation(cname, query), dictionary => dictionary.name));
}

const renderTranslation = (state, dispatch) => dname => {
  var translation = state.translations[state.category][dname];
  return (undefined === translation)
    ? null
    : (!translation.loaded)
      ? spinner('translation')
      : (
        <div class = "translation">
          <p>{dname}</p>
          <div class = "entries">
            {(!translation.entries.length)
             ? 'N/A'
             : translation.entries.map(entry => (
                <div class = "entry">
                  {entry}
                </div>
             ))}
          </div>
        </div>
      );
}


const renderTranslations = (state, dispatch) =>
  (undefined === state.translations || undefined === state.translations[state.category])
  ? null
  : (
    <div class = "translations">
      {state.dictionaries
       .filter(d => d.category == state.category && d.loaded && d.enabled)
       .map(compose(renderTranslation(state, dispatch), d => d.name))}
    </div>
  );


export {loadTranslation, loadTranslations, renderTranslation, renderTranslations};
