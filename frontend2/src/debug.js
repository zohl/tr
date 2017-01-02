import Inferno from 'inferno';
import {compose} from './common';


const initDebug = () => (state, _) => {
  state.debug = {
    enabled: true
  };
};


const guard = (test, f) => x => (test(x)) ? f(x) : undefined;
const otherwise = f => guard(_ => true, f);
const choose = (...gs) => x => {
  for (let i = 0; i < gs.length; ++i) {
    let result = gs[i](x);
    if (undefined !== result) {
      return result;
    }
  }
}


const isOfType = t => x => typeof x == t;

const isComposite = x => ("object" == typeof x && x !== null);


const toggleWatch = name => (state, dispatch) => {
  if (undefined === state.debug.watch) {
    state.debug.watch = {};
  }

  let result = name in state.debug.watch && state.debug.watch[name];
  state.debug.watch[name] = !result;
}


const renderComposite = (state, dispatch) => (path, obj) => Object.keys(obj)
      .filter(key => !(path == '' && 'debug' == key))
      .map(key => {
        var newPath = path + '.' + key;
        return (isComposite(obj[key])) ? (
         <label class = "object">
           <input type = "checkbox"
                  name = {newPath}
                  checked = {state.debug.watch && state.debug.watch[newPath]}
                  onInput = {compose(dispatch, toggleWatch, e => e.target.name)}
           />
           <div class = "widget"/>
           <p class = "field">{key}</p>
           <div class = "contents">
             {renderDebugStep(state, dispatch)(newPath, obj[key])}
           </div>
         </label>
        ): (<p class = "field">{key}: {renderDebugStep(state, dispatch)(path, obj[key])}</p>);
      });


const renderDebugStep = (state, dispatch) => (path, obj) => choose(
    guard(isOfType("string"), x => (<span class = "string">{x}</span>))
  , guard(isOfType("number"), x => (<span>{x.toString()}</span>))
  , guard(isOfType("boolean"), x => (<span>{x.toString()}</span>))
  , guard(isOfType("object"), choose(
      guard(x => x === null, _ => "(null)")
    , otherwise(x => renderComposite(state, dispatch)(path, x))))
  , otherwise(_ => (<span>'TODO'</span>)))(obj);


const renderDebug = (state, dispatch) => (!state.debug || !state.debug.enabled) ? '': (
  <div class = "debug">
    {renderDebugStep(state, dispatch)('', state)}
  </div>
);

export {initDebug, renderDebug};
