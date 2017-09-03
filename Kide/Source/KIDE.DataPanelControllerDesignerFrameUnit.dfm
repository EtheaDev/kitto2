inherited DataPanelControllerDesignerFrame: TDataPanelControllerDesignerFrame
  Height = 429
  HelpKeyword = 'DataPanelLeaf'
  ExplicitHeight = 429
  inherited ClientPanel: TPanel
    Height = 429
    ExplicitHeight = 429
    inherited DesignPanel: TPanel
      Height = 429
      ExplicitHeight = 429
      inherited ControllerGroupBox: TGroupBox
        Top = 43
        Height = 386
        TabOrder = 1
        ExplicitTop = 40
        ExplicitHeight = 389
        inherited ControllerPageControl: TPageControl
          Height = 312
          ExplicitHeight = 315
          inherited SubControllersTabSheet: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 24
            ExplicitWidth = 443
            ExplicitHeight = 287
            inherited SubControllersPageControl: TPageControl
              Height = 183
              ExplicitHeight = 186
              inherited CenterControllerTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 435
                ExplicitHeight = 158
              end
              inherited NorthControllerTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 435
                ExplicitHeight = 158
              end
              inherited EastControllerTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 435
                ExplicitHeight = 158
              end
              inherited SouthControllerTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 435
                ExplicitHeight = 158
              end
              inherited WestControllerTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 435
                ExplicitHeight = 158
              end
            end
          end
          inherited SubViewsTabSheet: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 24
            ExplicitWidth = 443
            ExplicitHeight = 287
            inherited SubViewsPageControl: TPageControl
              Height = 183
              ExplicitHeight = 186
              inherited CenterViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 435
                ExplicitHeight = 158
              end
              inherited NorthViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 435
                ExplicitHeight = 158
              end
              inherited EastViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 435
                ExplicitHeight = 158
              end
              inherited SouthViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 435
                ExplicitHeight = 158
              end
              inherited WestViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 435
                ExplicitHeight = 158
              end
            end
            inherited ViewsGroupBox: TGroupBox
              ExplicitTop = 0
            end
          end
        end
      end
      object DataPanelGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 455
        Height = 43
        Align = alTop
        Caption = 'TopToolbar'
        TabOrder = 0
        object TopToolBar: TToolBar
          Left = 2
          Top = 15
          Width = 451
          Height = 26
          AutoSize = True
          ButtonHeight = 26
          ButtonWidth = 26
          Images = MainDataModule.ToolbarImages
          TabOrder = 0
          object AddToolButton: TToolButton
            Left = 0
            Top = 0
            Hint = 'Allow/Prevent Adding (Default Allow)'
            ImageIndex = 0
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
          end
          object DupToolButton: TToolButton
            Left = 26
            Top = 0
            Hint = 'Allow/Prevent Duplicating  (Default Prevent)'
            ImageIndex = 1
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
          end
          object EditToolButton: TToolButton
            Left = 52
            Top = 0
            Hint = 'Allow/Prevent Editing (Default Allow)'
            ImageIndex = 2
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
          end
          object DeleteToolButton: TToolButton
            Left = 78
            Top = 0
            Hint = 'Allow/Prevent Deleting (Default Allow)'
            ImageIndex = 3
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
          end
          object ViewToolButton: TToolButton
            Left = 104
            Top = 0
            Hint = 'Allow/Prevent Viewing (Default Prevent)'
            ImageIndex = 4
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
          end
          object RefreshToolButton: TToolButton
            Left = 130
            Top = 0
            Hint = 'Allow/Prevent Refreshing (Default Allow)'
            ImageIndex = 5
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
          end
        end
      end
    end
  end
end
