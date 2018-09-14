function kittoInit()
{
  Ext.define('Kitto.Strings', {
    singleton: true,
    loadingMsg: 'Loading...'
  });

  Ext.override(Ext.Component, {
    getTopOwner: function() {
      var o = this.ownerCt;
      if (!o)
        return this;
      else
        return o.getTopOwner();
    }
  });

  Ext.override(Ext.Panel, {
    showFloating: function (isModal) {
      this.hostWindow = new Ext.Window({
        layout: "fit",
        title: this.title,
        width: this.width + 5,
        height: this.height + 5,
        header: true,
        closable: this.closable,
        onEsc: Ext.emptyFn,
        resizable: true,
        draggable: true,
        maximizable: false,
        iconCls: this.iconCls,
        style: this.style,
        modal: isModal,
        items: [this],
      });
      this.title = '';
      this.closable = false;
      this.header = false;
      this.setIconCls('');
      this.style = '';
      // In case it has custom close buttons...
      this.on('close', function() { this.hostWindow.close(); });
      this.hostWindow.show(getAnimationOrigin());
      if (!this.isVisible())
        this.show(getAnimationOrigin());
    },

    updateHostWindowTitle: function(title) {
      if (this.hostWindow)
        this.hostWindow.setTitle(title);
    }
  });

  Ext.override(Ext.TabPanel, {
    goPrevious: function() {
      var activeTab = this.getActiveTab();
      if (activeTab) {
        var previousTab = activeTab.previousSibling();
        if (previousTab) {
          this.setActiveTab(previousTab);
        }
      }
    },
    goNext: function() {
      var activeTab = this.getActiveTab();
      if (activeTab) {
      var nextTab = activeTab.nextSibling();
        if (nextTab) {
          this.setActiveTab(nextTab);
        }
      }
    }
  });

  // Used by TKExtForceUpperCase and siblings.
  Ext.override(Ext.form.Field, {

    setCursorPosition: function(pos) {
       var el = this.getEl().dom;
       if (el.createTextRange) {
          var range = el.createTextRange();
          range.move("character", pos);
          range.select();
       } else if(typeof el.selectionStart == "number" ) { 
          el.focus(); 
          el.setSelectionRange(pos, pos); 
       } else {
         alert('Method not supported');
       }
    },

    getCursorPosition: function() {
       var el = this.getEl().dom;
       var rng, ii=-1;
       if (typeof el.selectionStart=="number") {
          ii=el.selectionStart;
       } else if (document.selection && el.createTextRange){
          rng=document.selection.createRange();
          rng.collapse(true);
          rng.moveStart("character", -el.value.length);
          ii=rng.text.length;
       }
       return ii;
    },
    
    setValuePreservingCaretPos: function(v) {
      var p = this.getCursorPosition();
      this.setValue(v);
      this.setCursorPosition(p);
    }
  });

  // Used by TKExtForceCamelCaps.
  String.prototype.capitalize = function(){
    return this.replace( /(^|\s)([a-z])/g , function(m,p1,p2){ return p1+p2.toUpperCase(); } );
  };

  // Additional form vtypes.
  var
    codice_fiscale_re = /^[a-zA-Z]{6}[0-9]{2}[abcdehlmprstABCDEHLMPRST]{1}[0-9]{2}([a-zA-Z]{1}[0-9]{3})[a-zA-Z]{1}$/,
    partita_iva_re = /^[0-9]{11}$/,
    alpha_space_re = /^[a-zA-Z_ ]+$/;
    phone_number_re = /^\+[0-9]+\-[0-9]+$/
    digits_only_re = /^[0-9]/
  Ext.apply(Ext.form.VTypes, {
    codice_fiscale: function(val, field) {
      return codice_fiscale_re.test(val) || partita_iva_re.test(val);
    },
    codice_fiscaleText: 'Codice Fiscale non formalmente corretto.',
    codice_fiscaleMask: /[a-zA-Z0-9]/i,

    alpha_space: function(val, field) {
      return alpha_space_re.test(val);
    },
    alpha_spaceText: 'This field only accepts letters and spaces.',
    alpha_spaceMask: /[a-z ]/i,

    phone_number: function(val, field) {
      return phone_number_re.test(val);
    },
    phone_numberText: 'Invalid phone number.',
    phone_numberMask: /[a-z0-9\-\+]/,

    digits_only: function(val, field) {
      return digits_only_re.test(val);
    },
    digits_onlyText: 'This field accepts only digits.',
    digits_onlyMask: /[0-9]/
  });

  // Additional formats and renderers.
  Ext.apply(Ext.util.Format, {
    checkboxRenderer: function(val) {
      return String.format('<div class="x-grid3-check-col{0}"></div>', val ? "-on" : '');
    }
  });

  TextMetrics = new Ext.util.TextMetrics("body");
  Download = Ext.DomHelper.append(document.body, {tag: "iframe", cls: "x-hidden"});

  Ext.Ajax.on("beforerequest", function() { showKittoLoadMask(1); });
  Ext.Ajax.on("requestcomplete", function() { showKittoLoadMask(-1); });
  Ext.Ajax.on("requestexception", function() { showKittoLoadMask(0); });

  Ext.ariaWarn = Ext.emptyFn;

  if (!isMobileBrowser())
    Ext.tip.QuickTipManager.init();
  else
    Ext.tip.QuickTipManager.disable();

}
