function kittoInit()
{
  // Not used yet.
  Ext.override(Ext.Panel, {
    hideTbar: function() {
      this.tbar.setVisibilityMode(Ext.Element.DISPLAY);
      if (this.tbar.isVisible())
        this.tbar.fadeOut();
      //this.tbar.hide();
      //this.syncSize();
      //if (this.ownerCt)
      //  this.ownerCt.doLayout();
    },

    showTbar: function() {
      this.tbar.setVisibilityMode(Ext.Element.DISPLAY);
      if (!this.tbar.isVisible())
        this.tbar.fadeIn();
      //this.tbar.show();
      //this.syncSize();
      //if (this.ownerCt)
      //    this.ownerCt.doLayout();
    }
  });

  Ext.override(Ext.Component, {
    getTopOwner: function() {
      var o = this.ownerCt;
      if (!o)
        return this;
      else
        return o.getTopOwner();
    },

    getFormPanelOptimalSize: function() {
      var s;
      if (this.items)
      {
        for (var i = 0; i < this.items.getCount(); i++)
        {
          var item = this.items.get(i);
          if (item instanceof Ext.form.FormPanel)
            s = item.getOptimalSize();
          else
            s = item.getFormPanelOptimalSize();
          if (s)
            break;
        }
      }
      return s;
    }
  });

  Ext.override(Ext.form.FormPanel, {
    getOptimalSize: function() {
      var s = new Object;
      s.x = 0;
      s.y = 0;
      for (var i = 0; i < this.items.getCount(); i++)
      {
        var item = this.items.get(i);
        var w = 0;
        var h = 0;

        if (item instanceof Ext.Container && item.items && item.initialConfig.layout == "column")
        {
          for (var j = 0; j < item.items.getCount(); j++)
            w += item.items.get(j).getWidth();
          w += 24;
        }
        else
        {
          w = item.getEl().getRight() - this.getEl().getLeft();
        }
        h = item.getEl().getBottom() - this.getEl().getTop();
        if (w > s.x)
          s.x = w;
        if (h > s.y)
          s.y = h;
      }
      if (this.fbar)
      {
        s.y += this.fbar.getEl().getHeight();
      }
      return s;
    }
  });

  Ext.override(Ext.Window, {
    setClippedHeight: function(y) {
      ws = getWindowClientSize();
      if (y > ws.y)
        y = ws.y;
      this.setHeight(y);
      p = this.getPosition();
      this.setPosition(p[0], (ws.y / 2) - (y / 2));
    },

    setOptimalSize: function(extraWidth, extraHeight) {
      s = this.getFormPanelOptimalSize();
      if (s) {
        // Add space for borders and caption bar.
        // TODO: Calculate them automatically.
        s.x += 50 + extraWidth;
        s.y += extraHeight;
        s = clipToClientArea(s);
        this.setSize(s.x, s.y);
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
}
