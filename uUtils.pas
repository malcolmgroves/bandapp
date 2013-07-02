unit uUtils;

interface
function StripHtmlMarkup(const source:string):string;

implementation

function StripHtmlMarkup(const source:string):string;
var i, count: Integer;
    InTag: Boolean;
    P: PChar;
begin
  SetLength(Result, Length(source));
  P := PChar(Result);
  InTag := False;
  count := 0;
  for i:=0 to Length(source) - 1 do
  if InTag then
  begin
    if source[i] = '>' then
      InTag := False;
  end
  else
    if source[i] = '<' then
      InTag := True
    else
    begin
      P[count] := source[i];
      Inc(count);
    end;
  SetLength(Result, count);
end;

end.
