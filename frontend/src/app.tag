<app>
  <form class = "search">
    <input type = "input"
           placeholder = "Enter a word to translate"
           oninput = {onChangeQuery}
    />
  </form>

  <div>
    <label class = "dictionary" each = {dictionaries}>
        <input type = "checkbox"
               name = {name}
               onclick = {onChangeDictionary}
        />
        <p>{name} ({info.version})<p>
        <!-- <p>{info.bookName}</p>         -->
        <!-- <p>{info.wordCount}</p>        -->
        <!-- <p>{info.synWordCount}</p>     -->
        <!-- <p>{info.author}</p>           -->
        <!-- <p>{info.email}</p>            -->
        <!-- <p>{info.website}</p>          -->
        <!-- <p>{info.date}</p>             -->
        <!-- <p>{info.description}</p>      -->
    </label>
  </div>

  <div>
    <virtual each = {dictionaries}>
      <div if = {state == DICT_ENABLED && entries.length > 0} class = "translation">
        <h3>{name}</h3>
        <div class = "entry" each = {entry in entries}>
          <raw content = "{entry}"/>
          <hr/>
        </div>
      </div>
    </virtual>
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

      var done = entries => {
        dictionary.entries = entries;
        dictionary.state = this.DICT_ENABLED;
        this.update();
      };

      if (dictionary.state != this.DICT_DISABLED) {
        if (this.query.length == 0) {
          done([]);
        }
        else {
          let url = `/api/dictionary/${dictionary.name}/${this.query}`;
          let reqCount = dictionary.reqCount;

          fetch(url).then(r => r.json().then(entries => {
            if (dictionary.reqCount == reqCount) {
              done(entries);
            }
          }));
        }
      }
      else {
        this.update();
      }
    };

    onChangeQuery(e) {
      this.query = e.currentTarget.value;
      this.dictionaries.forEach(this.updateEntries);
    };

    onChangeDictionary(e) {
      this.dictionaries.forEach(d => {
        if (d.name == e.currentTarget.name) {
          d.state = (e.currentTarget.checked) ? this.DICT_PENDING : this.DICT_DISABLED;
          this.updateEntries(d);
        }
      });
    };

  </script>

</app>

