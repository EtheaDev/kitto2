inherited MainTableControllerDesignerFrame: TMainTableControllerDesignerFrame
  Width = 554
  Height = 588
  HelpKeyword = 'DataView'
  ExplicitWidth = 554
  ExplicitHeight = 588
  inherited ClientPanel: TPanel
    Width = 554
    Height = 588
    ExplicitWidth = 554
    ExplicitHeight = 588
    inherited DesignPanel: TPanel
      Width = 554
      Height = 588
      ExplicitWidth = 554
      ExplicitHeight = 588
      object PageControl: TPageControl
        Left = 0
        Top = 0
        Width = 554
        Height = 588
        ActivePage = ControllerTabSheet
        Align = alClient
        TabOrder = 0
        OnChange = PageControlChange
        object ControllerTabSheet: TTabSheet
          Caption = 'Controller'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object PopUpWindowGroupBox: TGroupBox
            Left = 0
            Top = 97
            Width = 546
            Height = 65
            Align = alTop
            Caption = 'PopUpWindow'
            TabOrder = 0
            object Label1: TLabel
              Left = 6
              Top = 16
              Width = 28
              Height = 13
              Caption = 'Width'
            end
            object Label2: TLabel
              Left = 66
              Top = 15
              Width = 31
              Height = 13
              Caption = 'Height'
            end
            object _PopUpWindow_Width: TSpinEdit
              Left = 6
              Top = 32
              Width = 54
              Height = 22
              Increment = 10
              MaxValue = 1920
              MinValue = -1
              TabOrder = 0
              Value = -1
            end
            object _PopUpWindow_Height: TSpinEdit
              Left = 66
              Top = 32
              Width = 54
              Height = 22
              Increment = 10
              MaxValue = 1080
              MinValue = -1
              TabOrder = 1
              Value = -1
            end
          end
          object GroupBox1: TGroupBox
            Left = 0
            Top = 0
            Width = 546
            Height = 97
            Align = alTop
            Caption = 'ViewTable'
            TabOrder = 1
            DesignSize = (
              546
              97)
            object PageRecordCountLabel: TLabel
              Left = 310
              Top = 16
              Width = 87
              Height = 13
              Caption = 'PageRecordCount'
            end
            object _AutoOpen: TCheckBox
              Left = 11
              Top = 15
              Width = 87
              Height = 17
              Caption = 'AutoOpen'
              TabOrder = 0
            end
            object _RowClassProvider: TLabeledEdit
              Left = 11
              Top = 69
              Width = 529
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              EditLabel.Width = 86
              EditLabel.Height = 13
              EditLabel.Caption = 'RowClassProvider'
              TabOrder = 4
            end
            object _PageRecordCount: TSpinEdit
              Left = 310
              Top = 31
              Width = 54
              Height = 22
              Increment = 10
              MaxValue = 1920
              MinValue = -1
              TabOrder = 3
              Value = -1
            end
            object _IsMultiSelect: TCheckBox
              Left = 104
              Top = 15
              Width = 88
              Height = 17
              Caption = 'IsMultiSelect'
              TabOrder = 1
            end
            object _PagingTools: TCheckBox
              Left = 197
              Top = 15
              Width = 97
              Height = 17
              Caption = 'PagingTools'
              TabOrder = 2
            end
          end
        end
        object GroupingTabSheet: TTabSheet
          Caption = 'Grouping'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object ToolViewsTabSheet: TTabSheet
          Caption = 'ToolViews'
          ImageIndex = 3
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object FormControllerTabSheet: TTabSheet
          Caption = 'FormController'
          ImageIndex = 3
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object FormTabSheet: TTabSheet
          Caption = 'Form'
          ImageIndex = 4
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object GridTabSheet: TTabSheet
          Caption = 'Grid'
          ImageIndex = 5
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
      end
    end
  end
end
