inherited TabPanelControllerDesignerFrame: TTabPanelControllerDesignerFrame
  Height = 402
  HelpKeyword = 'TabPanel'
  ExplicitHeight = 402
  inherited ClientPanel: TPanel
    Height = 402
    ExplicitHeight = 402
    inherited DesignPanel: TPanel
      Height = 402
      ExplicitHeight = 402
          inherited PanelControllerGroupBox: TGroupBox
            ExplicitTop = 44
          end
          object TabPanelControllerGroupBox: TGroupBox
            Left = 0
            Top = 140
            Width = 447
            Height = 40
            Align = alTop
            Caption = 'TabPanelController'
            TabOrder = 2
            ExplicitLeft = 3
            ExplicitTop = 208
            object _TabIconsVisible: TCheckBox
              Left = 11
              Top = 16
              Width = 114
              Height = 17
              Caption = 'TabIconsVisible'
              TabOrder = 0
            end
            object _TabsVisible: TCheckBox
              Left = 116
              Top = 16
              Width = 114
              Height = 17
              Caption = 'TabsVisible'
              TabOrder = 1
            end
          end
    end
  end
end
