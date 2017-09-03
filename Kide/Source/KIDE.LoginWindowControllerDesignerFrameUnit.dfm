inherited LoginWindowControllerDesignerFrame: TLoginWindowControllerDesignerFrame
  HelpKeyword = 'Login'
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      inherited ControllerGroupBox: TGroupBox
        object LoginPageControl: TPageControl
          Left = 2
          Top = 15
          Width = 451
          Height = 301
          ActivePage = WindowTabSheet
          Align = alClient
          TabOrder = 0
          OnChange = LoginPageControlChange
          object WindowTabSheet: TTabSheet
            Caption = 'Window'
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object ExtraWidthLabel: TLabel
              Left = 5
              Top = 3
              Width = 54
              Height = 13
              Caption = 'ExtraWidth'
            end
            object ExtraHeightLabel: TLabel
              Left = 90
              Top = 3
              Width = 57
              Height = 13
              Caption = 'ExtraHeight'
            end
            object LabelWidthLabel: TLabel
              Left = 169
              Top = 3
              Width = 53
              Height = 13
              Caption = 'LabelWidth'
            end
            object _ExtraWidth: TSpinEdit
              Left = 5
              Top = 18
              Width = 79
              Height = 22
              Increment = 10
              MaxValue = 0
              MinValue = 0
              TabOrder = 0
              Value = 0
            end
            object _ExtraHeight: TSpinEdit
              Left = 90
              Top = 18
              Width = 73
              Height = 22
              Increment = 10
              MaxValue = 0
              MinValue = 0
              TabOrder = 1
              Value = 0
            end
            object _LabelWidth: TSpinEdit
              Left = 169
              Top = 18
              Width = 73
              Height = 22
              MaxValue = 0
              MinValue = 0
              TabOrder = 2
              Value = 0
            end
          end
          object LocalStorageTabSheet: TTabSheet
            Caption = 'LocalStorage'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
          end
          object BorderPanelTabSheet: TTabSheet
            Caption = 'BorderPanel'
            ImageIndex = 2
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
          end
        end
      end
    end
  end
end
