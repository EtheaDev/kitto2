// Dynamically adds a style rule. Used to dynamically
// add icons and other CSS classes in async responses.
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
};

// Fires change event if the object value is at least
// minChars characters long. Also fires the event when the
// value is empty. Used in filters.
function fireChangeAfterNChars(obj, minChars)
{
  var v = obj.getValue();
  if (v.length >= minChars || v.length == 0)
  {
    obj.fireEvent("change", v, v);
    // Prevents firing of further change event
    // when focus leaves the control later.
    obj.startValue = v;
  }
};

// Calls an Ajax method if buttonId is "yes". The method to
// call is specified in obj.params.methodURL. The selection model
// specified in obj.params.selModel is used to get all values specified
// in obj.params.fieldNames from the first selected record and pass
// them in the Selection param to the Ajax method.
// This function should be used as a message box handler.
// All params specified above should be passed in the message
// box opt config, inside an object called params.
function ajaxSingleSelection(buttonId, text, obj)
{
  if (buttonId == "yes")
  {
    var
      selValues = [],
      selRecord = obj.params.selModel.getSelected(),
      fieldNames = obj.params.fieldNames.split(',');
    for (var i = 0; i < fieldNames.length; i++)
      selValues.push(fieldNames[i] + "=" + selRecord.get(fieldNames[i]));
    return Ext.Ajax.request({
      url: obj.params.methodURL,
      params: "Ajax=1&" + selValues.toString(),
      success: AjaxSuccess,
      failure: AjaxFailure
    });
  }
};

// Calls an Ajax method if buttonId is "yes". The method to
// call is specified in obj.params.methodURL.
// This function should be used as a message box handler.
// All params specified above should be passed in the message
// box opt config, inside an object called params.
function ajaxSimple(buttonId, text, obj)
{
  if (buttonId == "yes")
  {
    return Ext.Ajax.request({
      url: obj.params.methodURL,
      params: "Ajax=1",
      success: AjaxSuccess,
      failure: AjaxFailure
    });
  }
};

// Asks a confirmation message and calls a specified function
// when the dialog box is dismissed. Used together with ajaxSingleSelection
// and ajaxSimple.
function confirmCall(title, question, functionToCall, functionParams)
{
  Ext.Msg.show({
    title: title,
    msg: question,
    buttons: Ext.MessageBox.YESNO,
    icon: Ext.MessageBox.QUESTION,
    fn: functionToCall,
    params: functionParams
  });
};

// Formats a time specified as a "hh:mm:ss" string
// according to the specified format.
// Note: currently this function only decides whether to cut off the seconds
// part or not depending on the presence of "s" in the format string, which is
// just what we need now. It should be rewritten to support true formatting.
function formatTime(time, format)
{
  if ((!time) || (format.indexOf("s") > -1))
    return time;
  else
    return time.substring(0, 5);
};

// Renders an image with a value.
// Patterns is an array of arrays of two/three elements: image URL and regexp
// are mandatory, and the third element, if present, is a custom value template
// that can include v as the '{value}' placeholder.
// Patterns are searched in order.
// An image is rendered if v matches its regexp. Set includeValue to false
// to display only the image and not the value (if there's no matching image,
// the value is always displayed).
// Note: v is always used as the image tooltip (ext:qtip) and alt value.
function formatWithImage(v, patterns, includeValue)
{
  var image = null;
  var customValue = "";
  for (var i = 0; i < patterns.length; i++)
  {
    var re = new RegExp(patterns[i][1]);
    if (re.test(v)) {
      image = patterns[i][0];
      if (patterns[i].length >= 3)
        customValue = patterns[i][2];
      break;
    }
  }
  if (customValue != "")
    v = customValue.replace('{value}', v);
  if (image != null)
    // TODO: center image vertically?
    return '<img src="' + image + '" alt="' + v + '" ext:qtip="' + v + '">' + (includeValue ? '&nbsp;' + v : '');
  else
    return v;
};

// Matches value against a list of regexps.
// Patterns is an array of arrays of two elements: result and regexp.
// Patterns are searched in order.
// If a match is found, the corresponding result is returned (otherwise '').
function matchValue(value, patterns)
{
  var result = '';
  for (var i = 0; i < patterns.length; i++)
  {
    var re = new RegExp(patterns[i][1]);
    if (re.test(value)) {
      result = patterns[i][0];
      break;
    }
  }
  return result;
};

// Creates a style rule (see addStyleRule()) named after the
// color obtained by matching the record's specified field
// against the patterns (see matchValue()). The rule sets
// the background color to that color. Used to color grid rows
// according to field values.
function getRowColorStyleRule(record, fieldName, patterns)
{
  var color = matchValue(record.get(fieldName), patterns);
  if (color != '') {
    var ruleName = 'row-color-' + color;
    addStyleRule('.' + ruleName + ' { background-color: #' + color + '; }');
    return ruleName;
  }
  else
    return '';
};

function getWindowWidth() {
  return document.documentElement.clientWidth;
};

function getWindowHeight() {
  return document.documentElement.clientHeight;
};

function beforeChangeComponentSize(comp) {
  comp.kPreviousWidth = comp.getWidth();
  comp.kPreviousHeight = comp.getHeight();
};

function afterChangeComponentSize(comp) {
  if ("kPreviousWidth" in comp && comp.getTopOwner() instanceof Ext.Window)
  {
    var newWidth = comp.getTopOwner().getWidth() - comp.kPreviousWidth + comp.getWidth();
    if (newWidth > getWindowWidth())
      newWidth = getWindowWidth();
    comp.getTopOwner().setWidth(newWidth);
  }
  if ("kPreviousHeight" in comp && comp.getTopOwner() instanceof Ext.Window)
  {
    var newHeight = comp.getTopOwner().getHeight() - comp.kPreviousHeight + comp.getHeight();
    if (newHeight > getWindowHeight())
      newHeight = getWindowHeight();
    comp.getTopOwner().setHeight(newHeight);
    
    while (comp.getEl().getScroll().top > 0)
    {
      var newHeight = comp.getTopOwner().getHeight() + 10;
      if (newHeight >= getWindowHeight())
        break;
      comp.getTopOwner().setHeight(newHeight);
    }
  }
};
