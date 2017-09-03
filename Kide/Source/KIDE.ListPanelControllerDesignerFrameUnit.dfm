inherited ListPanelControllerDesignerFrame: TListPanelControllerDesignerFrame
  Height = 429
  HelpKeyword = 'List'
  ExplicitHeight = 429
  inherited ClientPanel: TPanel
    Height = 429
    ExplicitHeight = 429
    inherited DesignPanel: TPanel
      Height = 429
      ExplicitHeight = 429
      inherited ControllerGroupBox: TGroupBox
        Height = 429
        ExplicitHeight = 429
        inherited PanelControllerGroupBox: TGroupBox
          Top = 49
          ExplicitTop = 49
        end
        inherited ControllerPageControl: TPageControl
          Top = 106
          Height = 321
          ExplicitTop = 106
          ExplicitHeight = 321
          inherited SubControllersTabSheet: TTabSheet
            ExplicitHeight = 293
            inherited SubControllersPageControl: TPageControl
              Height = 192
              ExplicitHeight = 192
              inherited CenterControllerTabSheet: TTabSheet
                ExplicitHeight = 164
              end
              inherited NorthControllerTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 447
                ExplicitHeight = 367
              end
              inherited EastControllerTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 447
                ExplicitHeight = 367
              end
              inherited SouthControllerTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 447
                ExplicitHeight = 367
              end
              inherited WestControllerTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 447
                ExplicitHeight = 367
              end
            end
          end
          inherited SubViewsTabSheet: TTabSheet
            ExplicitHeight = 293
            inherited SubViewsPageControl: TPageControl
              Height = 192
              ExplicitHeight = 192
              inherited CenterViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 447
                ExplicitHeight = 367
              end
              inherited NorthViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 447
                ExplicitHeight = 367
              end
              inherited EastViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 447
                ExplicitHeight = 367
              end
              inherited SouthViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 447
                ExplicitHeight = 367
              end
              inherited WestViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 447
                ExplicitHeight = 367
              end
            end
            inherited ViewsGroupBox: TGroupBox
              ExplicitTop = 0
            end
          end
        end
        object ListGroupBox: TGroupBox
          Left = 2
          Top = 15
          Width = 451
          Height = 34
          Align = alTop
          Caption = 'List'
          TabOrder = 2
          object _AllowMultipleInstances: TCheckBox
            Left = 143
            Top = 14
            Width = 130
            Height = 16
            Caption = 'AllowMultipleInstances'
            TabOrder = 1
          end
          object _AllowClose: TCheckBox
            Left = 7
            Top = 14
            Width = 80
            Height = 16
            Caption = 'AllowClose'
            TabOrder = 0
          end
          object _InplaceEditing: TCheckBox
            Left = 327
            Top = 14
            Width = 90
            Height = 16
            Caption = 'InplaceEditing'
            TabOrder = 2
          end
        end
      end
    end
  end
end
