inherited GridPanelControllerDesignerFrame: TGridPanelControllerDesignerFrame
  Width = 554
  Height = 588
  HelpKeyword = 'GridPanel'
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
      inherited ControllerGroupBox: TGroupBox
        Width = 554
        Height = 545
        ExplicitWidth = 546
        ExplicitHeight = 560
        inherited PanelControllerGroupBox: TGroupBox
          Width = 550
          ExplicitWidth = 546
          inherited _Title: TLabeledEdit
            Width = 383
            ExplicitWidth = 383
          end
        end
        inherited ControllerPageControl: TPageControl
          Width = 550
          Height = 471
          ExplicitWidth = 550
          ExplicitHeight = 474
          inherited SubControllersTabSheet: TTabSheet
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 546
            ExplicitHeight = 560
            inherited SubControllersPageControl: TPageControl
              Width = 542
              Height = 342
              ExplicitWidth = 546
              ExplicitHeight = 560
              inherited CenterControllerTabSheet: TTabSheet
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 538
                ExplicitHeight = 532
              end
              inherited NorthControllerTabSheet: TTabSheet
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 538
                ExplicitHeight = 532
              end
              inherited EastControllerTabSheet: TTabSheet
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 538
                ExplicitHeight = 532
              end
              inherited SouthControllerTabSheet: TTabSheet
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 538
                ExplicitHeight = 532
              end
              inherited WestControllerTabSheet: TTabSheet
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 538
                ExplicitHeight = 532
              end
            end
            inherited ControllersGroupBox: TGroupBox
              Width = 542
              ExplicitWidth = 546
              inherited NorthControllerButton: TSpeedButton
                Width = 542
                ExplicitWidth = 542
              end
              inherited EastControllerButton: TSpeedButton
                Left = 444
                ExplicitLeft = 444
              end
              inherited SouthControllerButton: TSpeedButton
                Width = 542
                ExplicitWidth = 542
              end
              inherited CenterControllerButton: TSpeedButton
                Width = 342
                ExplicitWidth = 342
              end
            end
          end
          inherited SubViewsTabSheet: TTabSheet
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 546
            ExplicitHeight = 560
            inherited SubViewsPageControl: TPageControl
              Width = 542
              Height = 342
              ExplicitWidth = 546
              ExplicitHeight = 560
              inherited CenterViewTabSheet: TTabSheet
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 538
                ExplicitHeight = 532
              end
              inherited NorthViewTabSheet: TTabSheet
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 538
                ExplicitHeight = 532
              end
              inherited EastViewTabSheet: TTabSheet
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 538
                ExplicitHeight = 532
              end
              inherited SouthViewTabSheet: TTabSheet
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 538
                ExplicitHeight = 532
              end
              inherited WestViewTabSheet: TTabSheet
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 538
                ExplicitHeight = 532
              end
            end
            inherited ViewsGroupBox: TGroupBox
              Width = 542
              ExplicitWidth = 546
              inherited NorthViewButton: TSpeedButton
                Width = 542
                ExplicitWidth = 542
              end
              inherited EastViewButton: TSpeedButton
                Left = 444
                ExplicitLeft = 444
              end
              inherited SouthViewButton: TSpeedButton
                Width = 542
                ExplicitWidth = 542
              end
              inherited CenterViewButton: TSpeedButton
                Width = 342
                ExplicitWidth = 342
              end
            end
          end
        end
      end
      inherited DataPanelGroupBox: TGroupBox
        Width = 554
        ExplicitWidth = 546
      end
      object tsGrouping: TTabSheet
        Caption = 'Grouping'
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          554
          545)
        object _Grouping_SortFieldNames: TLabeledEdit
          Left = 304
          Top = 16
          Width = 206
          Height = 21
          Anchors = [akTop, akRight]
          EditLabel.Width = 74
          EditLabel.Height = 13
          EditLabel.Caption = 'SortFieldNames'
          TabOrder = 1
        end
        object _Grouping_FieldName: TLabeledEdit
          Left = 5
          Top = 16
          Width = 293
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 49
          EditLabel.Height = 13
          EditLabel.Caption = 'FieldName'
          TabOrder = 0
        end
        object _Grouping_EnableMenu: TCheckBox
          Left = 5
          Top = 45
          Width = 90
          Height = 17
          Caption = 'EnableMenu'
          TabOrder = 2
        end
        object _Grouping_StartCollapsed: TCheckBox
          Left = 94
          Top = 45
          Width = 90
          Height = 17
          Caption = 'StartCollapsed'
          TabOrder = 3
        end
        object _Grouping_ShowName: TCheckBox
          Left = 195
          Top = 45
          Width = 90
          Height = 17
          Caption = 'ShowName'
          TabOrder = 4
        end
        object ShowCountGroupBox: TGroupBox
          Left = 4
          Top = 71
          Width = 514
          Height = 92
          Anchors = [akLeft, akTop, akRight]
          Enabled = False
          TabOrder = 5
          DesignSize = (
            514
            92)
          object _Grouping_ShowCount_Template: TLabeledEdit
            Left = 7
            Top = 26
            Width = 499
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 44
            EditLabel.Height = 13
            EditLabel.Caption = 'Template'
            TabOrder = 0
          end
          object _Grouping_ShowCount_PluralItemName: TLabeledEdit
            Left = 7
            Top = 64
            Width = 287
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 75
            EditLabel.Height = 13
            EditLabel.Caption = 'PluralItemName'
            TabOrder = 1
          end
          object _Grouping_ShowCount_ItemName: TLabeledEdit
            Left = 300
            Top = 64
            Width = 206
            Height = 21
            Anchors = [akTop, akRight]
            EditLabel.Width = 49
            EditLabel.Height = 13
            EditLabel.Caption = 'ItemName'
            TabOrder = 2
          end
        end
        object _Grouping_ShowCount: TCheckBox
          Left = 11
          Top = 63
          Width = 79
          Height = 17
          Caption = 'ShowCount'
          TabOrder = 6
          OnClick = _Grouping_ShowCountClick
        end
      end
      object PopUpWindowTabSheet: TTabSheet
        Caption = 'PopUpWindow'
        ImageIndex = 1
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label1: TLabel
          Left = 5
          Top = 4
          Width = 28
          Height = 13
          Caption = 'Width'
        end
        object Label2: TLabel
          Left = 65
          Top = 3
          Width = 31
          Height = 13
          Caption = 'Height'
        end
        object _PopUpWindow_Width: TSpinEdit
          Left = 5
          Top = 20
          Width = 54
          Height = 22
          Increment = 10
          MaxValue = 1920
          MinValue = -1
          TabOrder = 0
          Value = -1
          OnKeyPress = IntegerKeyPress
        end
        object _PopUpWindow_Height: TSpinEdit
          Left = 65
          Top = 20
          Width = 54
          Height = 22
          MaxValue = 1080
          MinValue = -1
          TabOrder = 1
          Value = -1
          OnKeyPress = IntegerKeyPress
        end
      end
    end
  end
end
