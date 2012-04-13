function kittoInit()
{
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
}