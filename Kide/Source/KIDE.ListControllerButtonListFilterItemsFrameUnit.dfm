inherited ListControllerButtonListFilterItemsFrame: TListControllerButtonListFilterItemsFrame
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      object ItemsTabControl: TTabControl
        Left = 0
        Top = 0
        Width = 589
        Height = 299
        Align = alClient
        TabOrder = 0
        Tabs.Strings = (
          'Item')
        TabIndex = 0
        OnChange = ItemsTabControlChange
      end
    end
  end
end
