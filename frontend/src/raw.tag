<raw>
  this.content = '';

  var customUpdate = () => {
    if (this.content != opts.content) {
      this.root.innerHTML = opts.content;
      this.root.removeAttribute('content');
      this.content = opts.content;
    }
  };

  this.on('update', customUpdate);

  customUpdate();
</raw>

