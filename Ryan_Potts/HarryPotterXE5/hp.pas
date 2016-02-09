unit hp;

interface

uses System.SysUtils, System.Generics.Collections;

const hpBooks = '12345';

type

  Thp = class
    private
    public
    SingleBookPrice : extended;
    subBasket,
    Basket : string;
    Group  : TList<String>;
    function BasketTotal:extended;
    function DiscountPercentage:extended;
    function NumberOfDifferentBooks:integer;
    function GroupBasket:TList<String>;
    constructor Create;
  end;

implementation

constructor Thp.Create;
begin
  SingleBookPrice := 8;
  Basket := '';
  subBasket := '';
end;

function Thp.GroupBasket:TList<String>;
var lStrArray : TArray<String>;
    wrkBasket : string;
    tmpStr    : string;
    thisBook  : string;
    Index     : integer;
    StrCount  : integer;
begin
  wrkBasket := Basket;
  StrCount := 1;
  SetLength(lStrArray,StrCount);
  result := TList<String>.Create;
  thisBook := wrkBasket.Remove(1){removes "tail" of wrkBasket};
  while wrkBasket.Length > 0 do
  begin
    Index := 0;
    repeat
      tmpStr := lStrArray[Index];
      if thisBook.Length > 0 then
      begin
        if not tmpStr.Contains(thisBook) then
        begin
          tmpStr := tmpStr + thisBook;
          lStrArray[Index] := tmpStr;
          wrkBasket := wrkBasket.Remove(0,1){removes "head" of wrkBasket};
          thisBook := wrkBasket.Remove(1){removes "tail" of wrkBasket};
        end
        else
        if (Index = StrCount - 1) then
        begin
          inc(StrCount);
          SetLength(lStrArray,StrCount);
        end;
        inc(Index);
      end;
    until (Index = StrCount) or wrkBasket.IsEmpty;
  end;
  for tmpStr in lStrArray do
    result.Add(tmpStr);
end;

function Thp.BasketTotal:extended;
var hpBook : char;
    totalBooks : integer;
    subBaskets : TList<String>;
    wrkSubBasket : string;
    subTotal : extended;
begin
  subBaskets := GroupBasket;
  result := 0;
  for wrkSubBasket in subBaskets do
  begin
    subBasket := wrkSubBasket;
    totalBooks := 0;
    for hpBook in hpBooks do
      totalBooks := totalBooks + subBasket.CountChar(hpBook);
    subTotal := totalBooks * (SingleBookPrice * DiscountPercentage);
    result := result + subTotal;
  end;
end;

function Thp.DiscountPercentage:extended;
begin
  result := 1;
  case NumberOfDifferentBooks of
    2 : result := 0.95;
    3 : result := 0.9;
    4 : result := 0.8;
    5 : result := 0.75;
  end; //case
end;

function Thp.NumberOfDifferentBooks:integer;
var hpBook : char;
begin
  result := 0;
  for hpBook in hpBooks do
    if subBasket.Contains(hpBook) then
      inc(result);
end;

end.
