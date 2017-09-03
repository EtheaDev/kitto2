inherited FormPanelControllerDesignerFrame: TFormPanelControllerDesignerFrame
  Height = 429
  HelpKeyword = 'Form'
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
          Top = 67
          TabOrder = 2
          ExplicitTop = 67
        end
        inherited ControllerPageControl: TPageControl
          Top = 124
          Height = 303
          inherited SubControllersTabSheet: TTabSheet
            ExplicitHeight = 275
            inherited SubControllersPageControl: TPageControl
              Height = 174
              ExplicitHeight = 174
              inherited CenterControllerTabSheet: TTabSheet
                ExplicitHeight = 146
              end
              inherited NorthControllerTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 439
                ExplicitHeight = 373
              end
              inherited EastControllerTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 439
                ExplicitHeight = 373
              end
              inherited SouthControllerTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 439
                ExplicitHeight = 373
              end
              inherited WestControllerTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 439
                ExplicitHeight = 373
              end
            end
          end
          inherited SubViewsTabSheet: TTabSheet
            ExplicitHeight = 275
            inherited SubViewsPageControl: TPageControl
              Height = 174
              ExplicitHeight = 174
              inherited CenterViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 439
                ExplicitHeight = 373
              end
              inherited NorthViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 439
                ExplicitHeight = 373
              end
              inherited EastViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 439
                ExplicitHeight = 373
              end
              inherited SouthViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 439
                ExplicitHeight = 373
              end
              inherited WestViewTabSheet: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 439
                ExplicitHeight = 373
              end
            end
            inherited ViewsGroupBox: TGroupBox
              ExplicitTop = 213
            end
          end
        end
        object FormGroupBox: TGroupBox
          Left = 2
          Top = 15
          Width = 451
          Height = 52
          Align = alTop
          Caption = 'Form'
          TabOrder = 0
          object OperationLabel: TLabel
            Left = 239
            Top = 10
            Width = 48
            Height = 13
            Caption = 'Operation'
          end
          object _AllowMultipleInstances: TCheckBox
            Left = 7
            Top = 33
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
          object _IsModal: TCheckBox
            Left = 143
            Top = 14
            Width = 90
            Height = 16
            Caption = 'IsModal'
            TabOrder = 2
          end
          object _Operation: TComboBox
            Left = 239
            Top = 25
            Width = 74
            Height = 21
            TabOrder = 3
            Items.Strings = (
              'Add'
              'Edit')
          end
        end
      end
    end
  end
end
