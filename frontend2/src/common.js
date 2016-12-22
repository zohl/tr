export const update = (action, state) => action(state);

export const compose = (f, g) => x => f(g(x));
 
export const getJSON = (url, cb) => fetch(url).then(r => r.json().then(cb));

