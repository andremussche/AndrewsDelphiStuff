object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 531
  ClientWidth = 899
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 899
    Height = 490
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'DWS'
      object synedtDWS: TSynEdit
        Left = 0
        Top = 0
        Width = 891
        Height = 462
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Highlighter = SynDWSSyn1
        Lines.Strings = (
          '<html>'
          '<head>'
          '    <title>DwsRemoteChrome</title>'
          '</head>'
          ''
          '<script type="text/javascript">'
          ''
          '<%pas2js'
          ''
          '// this Pascal code will get compiled into JavaScript'
          ''
          'procedure SetPixel(x, y, c : Integer); external;'
          ''
          'procedure Mandel;'
          'const'
          '   cSize = 500;'
          'var'
          '   i, j, newColor : Integer;'
          '   u, v, x, y, z : Float;'
          'begin'
          '   for i := 0 to cSize-2 do begin'
          '      for j := 0 to cSize-2 do begin'
          '         x := -0.8 + 3 * i / cSize;'
          '         y := -1.4 + 2.8 * j / cSize;'
          '         newColor := 0;'
          '         u := 0;'
          '         v := 0;'
          '         repeat'
          '            z := Sqr(u) - Sqr(v) - x;'
          '            v := 2 * u * v - y;'
          '            u := z;'
          '            newColor := newColor + 1;'
          '         until (Sqr(u) + Sqr(v) > 9) or (newColor = 14);'
          '         SetPixel(i + 1, j + 1, newColor);'
          '      end;'
          '   end;'
          'end;'
          ''
          '%>'
          ''
          'var pixData;'
          ''
          
            'var cColors = [[0,0,0x22],[0,0,0x33],[0,0,0x44],[0,0,0x55],[0,0,' +
            '0x66],'
          
            '               [0,0,0x77],[0,0,0x88],[0x10,0x10,0x99],[0x30,0x30' +
            ',0xAA],'
          
            '               [0x40,0x40,0xBB],[0x50,0x50,0xCC],[0x60,0x60,0xDD' +
            '],'
          
            '               [0x70,0x70,0xEE],[0x80,0x80,0xFF],[0x00,0x00,0x00' +
            ']];'
          'function SetPixel(x,y,c) {'
          '  var p=(x+500*y)*4;'
          '  pixData[p]=cColors[c][0];'
          '  pixData[p+1]=cColors[c][1];'
          '  pixData[p+2]=cColors[c][2];'
          '  pixData[p+3]=255;'
          '}'
          ''
          'window.onload = function(){'
          '   calcAndShowMandel;'
          '   //recalc every 100ms'
          '   setInterval("calcAndShowMandel()", 1000);'
          ' }; '
          ' '
          'function calcAndShowMandel() {'
          '   var canvas = document.getElementById("canvas");'
          '   var ctx = canvas.getContext("2d");'
          '   var imageData = ctx.createImageData(500, 500);'
          '   pixData = imageData.data;'
          ''
          '   var t = new Date().getTime();'
          ''
          '   Mandel();'
          ''
          '   t = new Date().getTime()-t;'
          ''
          '   document.getElementById("time").innerHTML=t+" ms";'
          '   ctx.putImageData(imageData, 0, 0);   '
          '}'
          '</script>'
          ''
          '<body>'
          ''
          '<div style="text-align:center">'
          '   <div id="time"></div>'
          '   <canvas id="canvas" width="500" height="500"></canvas>'
          '</div>'
          ''
          '</body>'
          '</html>')
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Generate JS'
      ImageIndex = 1
      object synedtJS: TSynEdit
        Left = 0
        Top = 0
        Width = 891
        Height = 462
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Highlighter = SynJScriptSyn1
        Lines.Strings = (
          'synedtJS')
      end
    end
    object tsEmbeddedChrome: TTabSheet
      Caption = 'Embedded chrome'
      ImageIndex = 4
    end
    object tsRemoteDebug: TTabSheet
      Caption = 'Remote debug log'
      ImageIndex = 2
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 891
        Height = 462
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object tsRemoteJS: TTabSheet
      Caption = 'Remote JS'
      ImageIndex = 3
      object Splitter1: TSplitter
        Left = 185
        Top = 36
        Width = 5
        Height = 426
      end
      object Panel2: TPanel
        Left = 0
        Top = 36
        Width = 185
        Height = 426
        Align = alLeft
        TabOrder = 0
        object Label1: TLabel
          Left = 1
          Top = 1
          Width = 183
          Height = 13
          Align = alTop
          Caption = 'Stack:'
        end
        object Label2: TLabel
          Left = 1
          Top = 111
          Width = 183
          Height = 13
          Align = alTop
          Caption = 'Scope:'
        end
        object Label3: TLabel
          Left = 1
          Top = 221
          Width = 183
          Height = 13
          Align = alTop
          Caption = 'Properties of selected scope item:'
        end
        object lbStack: TListBox
          Left = 1
          Top = 14
          Width = 183
          Height = 97
          Align = alTop
          ItemHeight = 13
          TabOrder = 0
          OnDblClick = lbStackDblClick
        end
        object lbScope: TListBox
          Left = 1
          Top = 124
          Width = 183
          Height = 97
          Align = alTop
          ItemHeight = 13
          TabOrder = 1
          OnDblClick = lbScopeDblClick
        end
        object lbProperties: TListBox
          Left = 1
          Top = 234
          Width = 183
          Height = 191
          Align = alClient
          ItemHeight = 13
          TabOrder = 2
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 891
        Height = 36
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object btnPause: TButton
          Left = 3
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Pause'
          TabOrder = 0
          OnClick = btnPauseClick
        end
        object btnContinue: TButton
          Left = 81
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Continue'
          TabOrder = 1
          OnClick = btnContinueClick
        end
        object btnStepOver: TButton
          Left = 159
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Step over'
          TabOrder = 2
          OnClick = btnStepOverClick
        end
        object btnStepInto: TButton
          Left = 237
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Step into'
          TabOrder = 3
          OnClick = btnStepIntoClick
        end
      end
      object pgRemoteJS: TPageControl
        Left = 190
        Top = 36
        Width = 701
        Height = 426
        ActivePage = tsRemoteJS_DWS
        Align = alClient
        TabOrder = 2
        object tsRemoteJS_JS: TTabSheet
          Caption = 'JS'
          object synedtRemoteJS: TSynEdit
            Left = 0
            Top = 0
            Width = 693
            Height = 398
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 0
            OnDblClick = synedtRemoteJSDblClick
            OnMouseMove = synedtRemoteJSMouseMove
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.ShowLineNumbers = True
            Highlighter = SynJScriptSyn1
            Lines.Strings = (
              'synedtJS')
            Options = [eoAutoIndent, eoDragDropEditing, eoDropFiles, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
            ReadOnly = True
          end
        end
        object tsRemoteJS_DWS: TTabSheet
          Caption = 'DWS'
          ImageIndex = 1
          object synedtRemoteDWS: TSynEdit
            Left = 0
            Top = 0
            Width = 693
            Height = 398
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 0
            OnMouseMove = synedtRemoteDWSMouseMove
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.ShowLineNumbers = True
            Highlighter = SynDWSSyn1
            Lines.Strings = (
              '<html>'
              ''
              '<script type="text/javascript">'
              ''
              '<%pas2js'
              ''
              '// this Pascal code will get compiled into JavaScript'
              ''
              'procedure SetPixel(x, y, c : Integer); external;'
              ''
              'procedure Mandel;'
              'const'
              '   cSize = 500;'
              'var'
              '   i, j, newColor : Integer;'
              '   u, v, x, y, z : Float;'
              'begin'
              '   for i := 0 to cSize-2 do begin'
              '      for j := 0 to cSize-2 do begin'
              '         x := -0.8 + 3 * i / cSize;'
              '         y := -1.4 + 2.8 * j / cSize;'
              '         newColor := 0;'
              '         u := 0;'
              '         v := 0;'
              '         repeat'
              '            z := Sqr(u) - Sqr(v) - x;'
              '            v := 2 * u * v - y;'
              '            u := z;'
              '            newColor := newColor + 1;'
              '         until (Sqr(u) + Sqr(v) > 9) or (newColor = 14);'
              '         SetPixel(i + 1, j + 1, newColor);'
              '      end;'
              '   end;'
              'end;'
              ''
              '%>'
              ''
              'var pixData;'
              ''
              
                'var cColors = [[0,0,0x22],[0,0,0x33],[0,0,0x44],[0,0,0x55],[0,0,' +
                '0x66],'
              
                '               [0,0,0x77],[0,0,0x88],[0x10,0x10,0x99],[0x30,0x30' +
                ',0xAA],'
              
                '               [0x40,0x40,0xBB],[0x50,0x50,0xCC],[0x60,0x60,0xDD' +
                '],'
              
                '               [0x70,0x70,0xEE],[0x80,0x80,0xFF],[0x00,0x00,0x00' +
                ']];'
              'function SetPixel(x,y,c) {'
              '  var p=(x+500*y)*4;'
              '  pixData[p]=cColors[c][0];'
              '  pixData[p+1]=cColors[c][1];'
              '  pixData[p+2]=cColors[c][2];'
              '  pixData[p+3]=255;'
              '}'
              ''
              'window.onload = function(){'
              '   calcAndShowMandel;'
              '   //recalc every 100ms'
              '   setInterval("calcAndShowMandel()", 1000);'
              ' }; '
              ' '
              'function calcAndShowMandel() {'
              '   var canvas = document.getElementById("canvas");'
              '   var ctx = canvas.getContext("2d");'
              '   var imageData = ctx.createImageData(500, 500);'
              '   pixData = imageData.data;'
              ''
              '   var t = new Date().getTime();'
              ''
              '   Mandel();'
              ''
              '   t = new Date().getTime()-t;'
              ''
              '   document.getElementById("time").innerHTML=t+" ms";'
              '   ctx.putImageData(imageData, 0, 0);   '
              '}'
              '</script>'
              ''
              '<body>'
              ''
              '<div style="text-align:center">'
              '   <div id="time"></div>'
              '   <canvas id="canvas" width="500" height="500"></canvas>'
              '</div>'
              ''
              '</body>'
              '</html>')
            ReadOnly = True
          end
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 899
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btnCompileAndRun: TButton
      Left = 7
      Top = 9
      Width = 98
      Height = 25
      Caption = 'Compile and Run'
      TabOrder = 0
      OnClick = btnCompileAndRunClick
    end
    object btnConnect: TButton
      Left = 111
      Top = 9
      Width = 98
      Height = 25
      Caption = 'Remote Connect'
      TabOrder = 1
      OnClick = btnConnectClick
    end
    object btnLoadDebugScript: TButton
      Left = 213
      Top = 9
      Width = 100
      Height = 25
      Caption = 'load debug script'
      TabOrder = 2
      OnClick = btnLoadDebugScriptClick
    end
  end
  object SynJScriptSyn1: TSynJScriptSyn
    Left = 136
    Top = 168
  end
  object SynDWSSyn1: TSynDWSSyn
    DefaultFilter = 'DWScript Files (*.dws;*.pas;*.inc)|*.dws;*.pas;*.inc'
    Left = 216
    Top = 168
  end
  object IdHTTP1: TIdHTTP
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.CacheControl = 'max-age=0'
    Request.Connection = 'keep-alive'
    Request.ContentLength = -1
    Request.Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    Request.AcceptCharSet = 'ISO-8859-1,utf-8;q=0.7,*;q=0.3'
    Request.AcceptEncoding = 'gzip,deflate,sdch'
    Request.AcceptLanguage = 'en-US,en;q=0.8'
    Request.BasicAuthentication = False
    Request.UserAgent = 
      'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.19 (KHTML, l' +
      'ike Gecko) Chrome/18.0.1025.162 Safari/535.19'
    HTTPOptions = [hoForceEncodeParams]
    Left = 352
    Top = 160
  end
end
