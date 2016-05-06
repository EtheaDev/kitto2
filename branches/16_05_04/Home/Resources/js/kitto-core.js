// Returns the specified object with its x and y properties
// clipped to the window's client size.
function clipToClientArea(size) {
  s = size;
  ws = getWindowClientSize();
  if (s.x > ws.x)
    s.x = ws.x;
  if (s.y > ws.y)
    s.y = ws.y;
  return s;
}

// Returns the window's client size.
function getWindowClientSize() {
  var
    w = window,
    d = document,
    e = d.documentElement,
    g = d.getElementsByTagName('body')[0];
  var
    result = {};
  result.x = w.innerWidth || e.clientWidth || g.clientWidth,
  result.y = w.innerHeight|| e.clientHeight|| g.clientHeight;
  return result;
}

function getViewportWidth()
{
  return Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
}

function getViewportHeight()
{
  return Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
}

function getViewportWidthInInches()
{
  var div = document.createElement("div");
  div.style.width="1in";
  var body = document.getElementsByTagName("body")[0];
  body.appendChild(div);
  var ppi = document.defaultView.getComputedStyle(div, null).getPropertyValue('width');
  body.removeChild(div);
  var w = getViewportWidth() / parseFloat(ppi);
  return Math.round(w);
}

// Used to avoid displaying dialog boxes larger that the viewport in mobile browsers.
function getMaxMsgWidth()
{
  return Math.min(600, getViewportWidth() - 6);
}

// Used to make dialog boxes as large as the viewport in mobile browsers.
function getMinMsgWidth()
{
  return isMobileBrowser() ? getMaxMsgWidth() : 100;
}

// Dynamically adds a style rule. Used to dynamically
// add icons and other CSS classes in async responses.
function addStyleRule(selector, rule) {
  existingRule = Ext.util.CSS.getRule(selector, false);
  
  if (!existingRule)
    return Ext.util.CSS.createStyleSheet(selector + ' ' + rule);
  else
    return null;
};

// Dynamically adds a css <link> that references a URL. Used to
// dynamically add a style sheet specific to a view or controller.
function addLinkRef(href) {
  var
    head = document.getElementsByTagName('head')[0],
    link = document.createElement('link');
  link.rel = 'stylesheet';
  link.type = 'text/css';
  link.href = href;
  head.appendChild(link);
};

// Dynamically adds a <script> that references a URL. Used to
// dynamically add javascript code specific to a view or controller.
function addScriptRef(src) {
  var
    head = document.getElementsByTagName('head')[0],
    script = document.createElement('script');
  script.type = 'text/javascript';
  script.src = src;
  head.appendChild(script);
};

// Fires change event if the object's value is at least
// minChars characters long. Also fires the event when the
// value is empty. Used in filters.
function fireChangeAfterNChars(obj, minChars)
{
  if (!Ext.Ajax.isLoading())
  {
    var v = obj.getValue();
    if (v.length >= minChars || v.length == 0)
    {
      obj.fireEvent("change", v, v);
      // Prevents firing of further change event
      // when focus leaves the control later.
      obj.startValue = v;
    }
  }
};

function fireChangeIfEmpty(obj)
{
  var v = obj.getRawValue();
  if (v.length == 0)
  {
    obj.fireEvent("change", v, v);
  }
};

// Calls an Ajax method if buttonId is "yes". The method to
// call is specified in obj.params.methodURL. The selection model
// specified in obj.params.selModel is used to get all values specified
// in obj.params.fieldNames from all selected records and pass
// them in the Selection param to the Ajax method.
// This function should be used as a message box handler.
// All params specified above should be passed in the message
// box opt config, inside an object called params.
function ajaxSelection(buttonId, text, obj)
{
  if (buttonId == "yes")
  {
    var
      selValues = [],
      selRecords = obj.params.selModel.getSelections(),
      fieldNames = obj.params.fieldNames.split(',');
    for (var i = 0; i < fieldNames.length; i++)
    {
      var fieldValues = [];
      for (var j = 0; j < selRecords.length; j++)
        fieldValues.push(selRecords[j].get(fieldNames[i]));
      selValues.push(fieldNames[i] + "=" + fieldValues.toString());
    }
    return Ext.Ajax.request({
      url: obj.params.methodURL,
      params: "Ajax=1&" + selValues.join('&'),
      success: AjaxSuccess,
      failure: AjaxFailure
    });
  }
};

// Calls an Ajax method if buttonId is "yes". The method to
// call is specified in obj.params.methodURL. The data view
// specified in obj.params.dataView is used to get all values specified
// in obj.params.fieldNames from all selected records and pass
// them in the Selection param to the Ajax method.
// This function should be used as a message box handler.
// All params specified above should be passed in the message
// box opt config, inside an object called params.
function ajaxDataViewSelection(buttonId, text, obj)
{
  if (buttonId == "yes")
  {
    var
      selValues = [],
      selRecords = obj.params.dataView.getSelectedRecords(),
      fieldNames = obj.params.fieldNames.split(',');
    for (var i = 0; i < fieldNames.length; i++)
    {
      var fieldValues = [];
      for (var j = 0; j < selRecords.length; j++)
        fieldValues.push(selRecords[j].get(fieldNames[i]));
      selValues.push(fieldNames[i] + "=" + fieldValues.toString());
    }
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
// when the dialog box is dismissed. To be used with functions like ajaxSimple.
function confirmCall(title, question, functionToCall, functionParams)
{
  showMessage({
    title: title,
    msg: question,
    buttons: Ext.MessageBox.YESNO,
    icon: Ext.MessageBox.QUESTION,
    fn: functionToCall,
    params: functionParams
  });
};

function expandObjectProperties(template, object)
{
  var s = template;
  for (var p in object)
    s = s.replace('{' + p + '}', object[p]);
  return s;
}

// Asks a confirmation message and calls ajaxSelection
// when the dialog box is dismissed. The question is built by replacing
// the {caption} token in questionTpl with the value of captionFieldName
// in the last selected record in selModel.
// Any {fieldname} tokens are also replaced with values from the same record.
function selectConfirmCall(title, questionTpl, selModel, captionFieldName, functionParams)
{
  var lastRecord = selModel.getSelections().slice(-1)[0];
  var processedMsg = (captionFieldName !== "") && (questionTpl.indexOf("{caption}") !== -1) ? questionTpl.replace("{caption}", lastRecord.get(captionFieldName).toString()) : questionTpl;
  processedMsg = expandObjectProperties(processedMsg, lastRecord.data);

  showMessage({
    title: title,
    msg: processedMsg,
    buttons: Ext.MessageBox.YESNO,
    icon: Ext.MessageBox.QUESTION,
    fn: ajaxSelection,
    params: functionParams
  });
};

// Asks a confirmation message and calls ajaxDataViewSelection
// when the dialog box is dismissed. The question is built by replacing
// the {caption} token in questionTpl with the value of captionFieldName
// in the last selected record in selModel.
function selectDataViewConfirmCall(title, questionTpl, dataView, captionFieldName, functionParams)
{
  var lastRecord = dataView.getSelectedRecords()[0];
  var processedMsg = (captionFieldName !== "") && (questionTpl.indexOf("{caption}") !== -1) ? questionTpl.replace("{caption}", lastRecord.get(captionFieldName).toString()) : questionTpl;
  processedMsg = expandObjectProperties(processedMsg, lastRecord.data);
  
  showMessage({
    title: title,
    msg: processedMsg,
    buttons: Ext.MessageBox.YESNO,
    icon: Ext.MessageBox.QUESTION,
    fn: ajaxDataViewSelection,
    params: functionParams
  });
};

// Formats a time specified as a "hh:mm:ss" string according to the specified format.
// Note: currently this function only decides whether to cut off the seconds
// part or not depending on the presence of "s" in the format string, and whether
// to use ":" or "." as a time separator. Nothing else is implemented for now.
function formatTime(time, format)
{
  var resultTime = time;
  // Set the right time separator.
  if (resultTime) {
    if (format.indexOf(":") > -1)
      resultTime = resultTime.replace(/\./g, ':');
    else if (format.indexOf(".") > -1)
      resultTime = resultTime.replace(/:/g, '.');
  }
  // Format with or without seconds.
  if ((!resultTime) || (format.indexOf("s") > -1))
    return resultTime;
  else
    return resultTime.substring(0, 5);
};

// Returns a string equal to displayTemplate with all occurrences of
// "{value}" replaced by the specificed v. If no displayTemplate is
// specified, then returns v.
function formatWithDisplayTemplate(v, displayTemplate)
{
  if (displayTemplate != "")
    return displayTemplate.replace('{value}', v);
  else
    return v;
}

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
  {
    var style = (includeValue ? '': 'display:block;margin-left:auto;margin-right:auto');
    // TODO: center image vertically?
    return '<img style="' + style + '" src="' + image + '" alt="' + v + '" ext:qtip="' + v + '">' + (includeValue ? '&nbsp;' + v : '');
  }
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
// 'kitto-color-' prefix and the color obtained by matching the
// record's specified field against the patterns (see matchValue()).
// The rule sets the background color to that color.
// Used to color grid cells and rows according to field values.
function getColorStyleRuleForRecordField(record, fieldName, patterns)
{
  var color = matchValue(record.get(fieldName), patterns);
  if (color != '') {
    var ruleName = 'kitto-color-' + color;
    addStyleRule('.' + ruleName, '{ background-color: #' + color + '; }');
    return ruleName;
  }
  else
    return '';
};

// Creates a style rule (see addStyleRule()) named after the
// 'kitto-color-' prefix and the color obtained by matching the
// specified value against the patterns (see matchValue()).
// The rule sets the background color to that color.
// Used to color grid cells and rows according to field values.
function getColorStyleRuleForValue(value, patterns)
{
  var color = matchValue(value, patterns);
  if (color != '') {
    var ruleName = 'kitto-color-' + color;
    addStyleRule('.' + ruleName, '{ background-color: #' + color + '; }');
    return ruleName;
  }
  else
    return '';
};

function getWindowWidth()
{
  return document.documentElement.clientWidth;
};

function getWindowHeight()
{
  return document.documentElement.clientHeight;
};

function loadError(type, action, response)
{
  if (type == "remote")
    AjaxError("Load error: " + response.message);
  else
    AjaxError("Load error - see response for details.");
};

function objectToParams(object)
{
  return Object.keys(object).map(
    function(k) {
      return encodeURIComponent(k) + '=' + encodeURIComponent(object[k]);
    }
  ).join('&');
};

window.kittoLoadMaskShowCount = 0;

// shows (1) or hides (-1) the loading mask.
// The mask is shown as long as the current sum of show calls
// is > 0, hidden otherwise. So the 1 and -1 calls must be balanced.
// Pass 0 to reset the counter.
function showKittoLoadMask(amount)
{
  if (amount == 0)
    window.kittoLoadMaskShowCount = 0;
  else
    window.kittoLoadMaskShowCount += amount;
  if (window.kittoLoadMaskShowCount > 0)
    kittoLoadMask.show();
  else
    kittoLoadMask.hide();
}

function isMobileBrowser()
{
  return /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);
}

// A shortcut for Ext.Msg.show that enforces dialog size.
function showMessage(config)
{
  config.maxWidth = getMaxMsgWidth();
  config.minWidth = getMinMsgWidth();
  return Ext.Msg.show(config);
}
