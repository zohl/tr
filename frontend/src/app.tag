<app>
  <div class = "container">

    <form class = "search">
      <input type = "input" placeholder = "_" oninput = {onChangeQuery}/>
    </form>

    <div class = "dictionaries">
      <div class = "dictionary" each = {dictionaries}>
        <div class = "header">
          <label class = "control-enable">
            <input type = "checkbox" name = {name} onclick = {onChangeDictionary}/>
            <div class = "widget"/>
            <div class = "name">{name}&nbsp;({info.version})</div>
          </label>
          <label class = "control-info">
            <input type = "checkbox" ref = "{name}"/>
            <div class = "widget"/>
          </label>
        </div>

        <div class = "info">
          <p>{info.bookName}</p>
          <p>{info.wordCount}</p>
          <p>{info.synWordCount}</p>
          <p>{info.author}</p>
          <p>{info.email}</p>
          <p>{info.website}</p>
          <p>{info.date}</p>
          <p>{info.description}</p>
        </div>
      </div>
    </div>

    <div class = "translations">
      <virtual each = {dictionaries}>
        <div if = {state == DICT_PENDING} class = "translation">
          ...loading...
        </div>
        <div if = {state == DICT_ENABLED && entries.length > 0} class = "translation">
          <h3>{name}</h3>
          <div class = "entry" each = {entry in entries}>
            <raw content = "{entry}"/>
            <hr/>
          </div>
        </div>
      </virtual>
    </div>
  </div>

  <script>
    this.DICT_DISABLED = 0;
    this.DICT_ENABLED  = 1;
    this.DICT_PENDING  = 2;

    this.query = "";
    this.dictionaries = [];

    fetch('/api/dictionary').then(r => r.json().then(data => {
      data.forEach(name => {
        fetch(`/api/dictionary/${name}`).then(r => r.json().then(info => {
          this.dictionaries.push({
              name: name
            , state: this.DICT_DISABLED
            , info: info
            , entries: []
            , reqCount: 0
          });
          this.update();
        }));
      });
    }));


    this.updateEntries = dictionary => {
      ++dictionary.reqCount;
      dictionary.state = this.DICT_PENDING;

      var done = entries => {
        dictionary.entries = entries;
        dictionary.state = this.DICT_ENABLED;
        this.update();
      };

      if (this.query.length == 0) {
        done([]);
      }
      else {
        let url = `/api/dictionary/${dictionary.name}/${this.query}`;
        let reqCount = dictionary.reqCount;

        fetch(url).then(r => r.json().then(entries => {
          if (dictionary.reqCount == reqCount && dictionary.state != this.DICT_DISABLED) {
            done(entries);
          }
        }));
      }
    };

    onChangeQuery(e) {
      this.query = e.currentTarget.value;
      this.dictionaries.forEach(d => {
        if (d.state != this.DICT_DISABLED) {
          this.updateEntries(d);
        }
      });
      this.update();
    };

    onChangeDictionary(e) {
      this.dictionaries.forEach(d => {
        if (d.name == e.currentTarget.name) {
          if (e.currentTarget.checked) {
            this.updateEntries(d);
          }
          else {
            d.state = this.DICT_DISABLED;
          }
        }
      });
    };

  </script>

</app>

