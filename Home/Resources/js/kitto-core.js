// Dynamically adds a style rule. Used to add icons
// through CSS in async responses.
function addStyleRule(rule) {
  var head = document.getElementsByTagName('head')[0],
      style = document.createElement('style'),
      rules = document.createTextNode(rule);
  style.type = 'text/css';
  if (style.styleSheet)
    style.styleSheet.cssText = rules.nodeValue;
  else
    style.appendChild(rules);
  head.appendChild(style);
}

// Used by ForceCase=caps
String.prototype.capitalize = function(){
   return this.replace( /(^|\s)([a-z])/g , function(m,p1,p2){ return p1+p2.toUpperCase(); } );
};

// Additional form vtypes.
var
  codice_fiscale_re = /^[a-zA-Z]{6}[0-9]{2}[abcdehlmprstABCDEHLMPRST]{1}[0-9]{2}([a-zA-Z]{1}[0-9]{3})[a-zA-Z]{1}$/,
  partita_iva_re = /^[0-9]{11}$/,
  alpha_space_re = /^[a-zA-Z_ ]+$/;

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
    alpha_spaceMask: /[a-z ]/i
});

// Fires change event if the object value is at least
// minChars characters long. Used in filters.
function fireChangeAfterNChars(obj, minChars)
{
  var v = obj.getValue();
  if (v.length >= minChars)
    obj.fireEvent("change", v, v);
}
