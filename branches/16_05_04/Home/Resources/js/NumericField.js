/**
 * Copyright(c) 2011
 *
 * Licensed under the terms of the Open Source LGPL 3.0
 * http://www.gnu.org/licenses/lgpl.html
 * @author Greivin Britton, brittongr@gmail.com
 *     
 * @changes
 * No currency symbol by default    
 * No decimalPrecision in config
 * Supporting any character as thousand separator
 * Improved getFormattedValue
 * Removed unnecessary code to create format template, now using float.toFixed(this.decimalPrecission)    
 */
	
Ext.ux.NumericField = function(config){
	var defaultConfig = 
    {
		style: 'text-align:right;'
    };
    
    Ext.ux.NumericField.superclass.constructor.call(this, Ext.apply(defaultConfig, config));

    //Only if thousandSeparator doesn't exists is assigned when using decimalSeparator as the same as thousandSeparator
	if(this.useThousandSeparator && this.decimalSeparator == ',' && Ext.isEmpty(config.thousandSeparator))
		this.thousandSeparator = '.';
	else
		if(this.allowDecimals && this.thousandSeparator == '.' && Ext.isEmpty(config.decimalSeparator))
			this.decimalSeparator = ',';
		
    this.onFocus = this.onFocus.createSequence(this.onFocus);
};

Ext.extend(Ext.ux.NumericField, Ext.form.NumberField, 
{
    currencySymbol: null,
	useThousandSeparator: true,
	thousandSeparator: ',',
	alwaysDisplayDecimals: false,
	setValue: function(v){
	   Ext.ux.NumericField.superclass.setValue.call(this, v);
       
	   this.setRawValue(this.getFormattedValue(this.getValue()));
    },
	/**
	 * No more using Ext.util.Format.number, Ext.util.Format.number in ExtJS versions
	 * less thant 4.0 doesn't allow to use a different thousand separator than "," or "."
	 * @param {Number} v
	 */
    getFormattedValue: function(v){
       
		if (Ext.isEmpty(v) || !this.hasFormat()) 
            return v;
	    else 
        {
			var neg = null;
			
			v = (neg = v < 0) ? v * -1 : v;	
			v = this.allowDecimals && this.alwaysDisplayDecimals ? v.toFixed(this.decimalPrecision) : v;
			
			if(this.useThousandSeparator)
			{
				if(this.useThousandSeparator && Ext.isEmpty(this.thousandSeparator))
					throw ('NumberFormatException: invalid thousandSeparator, property must has a valid character.');
				
				if(this.thousandSeparator == this.decimalSeparator)
					throw ('NumberFormatException: invalid thousandSeparator, thousand separator must be different from decimalSeparator.');
				
				var v = String(v);
		
				var ps = v.split('.');
                ps[1] = ps[1] ? ps[1] : null;
                
                var whole = ps[0];
                
                var r = /(\d+)(\d{3})/;

				var ts = this.thousandSeparator;
				
                while (r.test(whole)) 
                    whole = whole.replace(r, '$1' + ts + '$2');
            
			    v = whole + (ps[1] ? this.decimalSeparator + ps[1] : '');
			}
			
			return String.format('{0}{1}{2}', (neg ? '-' : ''), (Ext.isEmpty(this.currencySymbol) ? '' : this.currencySymbol + ' '), v);
        }
    },
    /**
     * overrides parseValue to remove the format applied by this class
     */
    parseValue: function(v){
		//Replace the currency symbol and thousand separator
        return Ext.ux.NumericField.superclass.parseValue.call(this, this.removeFormat(v));
    },
    /**
     * Remove only the format added by this class to let the superclass validate with it's rules.
     * @param {Object} v
     */
    removeFormat: function(v){
        if (Ext.isEmpty(v) || !this.hasFormat()) 
            return v;
        else 
        {
			v = v.replace(this.currencySymbol + ' ', '');
			
			v = this.useThousandSeparator ? v.replace(new RegExp('[' + this.thousandSeparator + ']', 'g'), '') : v;
			//v = this.allowDecimals && this.decimalPrecision > 0 ? v.replace(this.decimalSeparator, '.') : v;
			
            return v;
        }
    },
    /**
     * Remove the format before validating the the value.
     * @param {Number} v
     */
    getErrors: function(v){
        return Ext.ux.NumericField.superclass.getErrors.call(this, this.removeFormat(v));
    },
	hasFormat: function()
	{
		return this.decimalSeparator != '.' || this.useThousandSeparator == true || !Ext.isEmpty(this.currencySymbol) || this.alwaysDisplayDecimals;	
	},
    /**
     * Display the numeric value with the fixed decimal precision and without the format using the setRawValue, don't need to do a setValue because we don't want a double
     * formatting and process of the value because beforeBlur perform a getRawValue and then a setValue.
     */
    onFocus: function(){
		this.setRawValue(this.removeFormat(this.getRawValue()));
    }
});
Ext.reg('numericfield', Ext.ux.NumericField);