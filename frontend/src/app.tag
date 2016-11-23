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
        <p>{name}<p>
        <!-- <p>{version}</p>          -->
        <!-- <p>{bookName}</p>         -->
        <!-- <p>{wordCount}</p>        -->
        <!-- <p>{synWordCount}</p>     -->
        <!-- <p>{author}</p>           -->
        <!-- <p>{email}</p>            -->
        <!-- <p>{website}</p>          -->
        <!-- <p>{description}</p>      -->
        <!-- <p>{date}</p>             -->
        <p>{enabled}</p>
    </label>
  </div>

  <div>
    <div class = "translation" each = {translations}>
      <h3>{name}</h3>
      <div class = "entry" each = {entry in entries}>
        <p>{entry}</p>
        <hr/>
      </div>
    </div>
  </div>

  <script>
    this.query = "";
    this.dictionaries = [];
    this.translations = [];

    this.updateTranslations = () => {
      this.translations = [];
      this.dictionaries.forEach(d => {
        if (d.enabled) {
          fetch('/api/dictionary/'+d.name+'/'+this.query).then(r => r.json().then(data => {
            if (data.length > 0) {
              this.translations.push({name: d.name, entries: data});
              this.update();
            }
          }));
        }
      });
    }

    onChangeQuery(e) {
      this.query = e.currentTarget.value;
      this.updateTranslations();
    };

    onChangeDictionary(e) {
      this.dictionaries.forEach(d => {
        if (d.name == e.currentTarget.name) {
          d.enabled = e.currentTarget.checked;
          this.updateTranslations();
        }
      });
    };

    fetch('/api/dictionary').then(r => r.json().then(data => {
      data.forEach(name => {
        fetch('/api/dictionary/' + name).then(response => response.json().then(info => {
          this.dictionaries.push(Object.assign(info, {name: name, enabled: false}));
          this.update();
        }));
      });
    }));

  </script>

</app>

