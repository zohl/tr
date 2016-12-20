import Inferno from 'inferno';

var onChangeQuery = function() {
  // TODO
}

export default function app() {
  return (
    <div className = "container">
      <form className = "search">
        <input type = "input" placeholder = "_" onChange = {onChangeQuery}/>
      </form>

      <div className = "dictionaries">TODO</div>
      <div className = "translations">TODO</div>
    </div>
  );
}

