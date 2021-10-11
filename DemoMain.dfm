object Form1: TForm1
  Left = 232
  Top = 124
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Script engine'
  ClientHeight = 202
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 16
  object Memo1: TMemo
    Left = 8
    Top = 40
    Width = 433
    Height = 153
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      'ShowMessage("I like Delphi!")'
      'Add(5,2,16,135,15,1,-3)'
      
        'ShowMessage("RTTI rules! This application will close in 3 second' +
        's")'
      'CloseApp(3)')
    ParentFont = False
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 433
    Height = 25
    Caption = 'Run Script'
    TabOrder = 1
    OnClick = Button1Click
  end
end
