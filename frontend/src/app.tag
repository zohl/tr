<app>
  <form class = "search">
    <input type = "input"
           placeholder = "Enter a word to translate"
           oninput = {getTranslation}
    />
  </form>

  <label class = "dictionary" each = {dictionaries}>
      <input type = "checkbox"/>
      <p>{name}<p>
      <p>{version}</p>
      <p>{bookName}</p>
      <p>{wordCount}</p>
      <p>{synWordCount}</p>
      <p>{author}</p>
      <p>{email}</p>
      <p>{website}</p>
      <p>{description}</p>
      <p>{date}</p>
  </label>

  <div class = "translation" each = {translations}>
    <h3>{name}</h3>
    <div class = "entry" each = {entry in entries}>
      <p>{entry}</p>
      <hr/>
    </div>
  </div>

  <script>
    this.query = "";
    this.dictionaries = [];
    this.translations = [];

    var getTranslation = function(e) {
      var value = e.currentTarget.value;
      console.log(value);
      this.translations = [];
    };

    fetch('/api/dictionary').then(response => response.json().then(data => {
      data.forEach(name => {
        fetch('/api/dictionary/' + name).then(response => response.json().then(info => {
          this.dictionaries.push(Object.assign(info, {name: name}));
          this.update();
        }));
      });
    }));

  </script>

</app>

