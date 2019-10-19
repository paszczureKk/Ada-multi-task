-- Szkielet programu do zadania z języków programowania
-- Studenci powinni przemianować zadania producentów, konsumentów i bufora
-- Powinni następnie zmienić je tak, by odpowiadały ich własnym zadaniom
-- Powinni także uzupełnić kod o brakujące konstrucje
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;


procedure Symulacja is

   Liczba_Wyrobow: constant Integer := 5;
   Liczba_Zestawow: constant Integer := 3;
   Liczba_Konsumentow: constant Integer := 2;

   subtype Zakres_Czasu_Produkcji is Integer range 3 .. 6;
   subtype Zakres_Czasu_Konsumpcji is Integer range 4 .. 8;

   subtype Typ_Wyrobow is Integer range 1 .. Liczba_Wyrobow;
   subtype Typ_Zestawow is Integer range 1 .. Liczba_Zestawow;
   subtype Typ_Konsumenta is Integer range 1 .. Liczba_Konsumentow;

   Nazwa_Wyrobu: constant array (Typ_Wyrobow) of Unbounded_String
     := (
         To_Unbounded_String("Bulka"),
         To_Unbounded_String("Kielbasa"),
         To_Unbounded_String("Salata"),
         To_Unbounded_String("Pomidor"),
         To_Unbounded_String("Sos")
        );
   Nazwa_Zestawu: constant array (Typ_Zestawow) of Unbounded_String
     := (
         To_Unbounded_String("Hot-Dog"),
         To_Unbounded_String("Kanapka"),
         To_Unbounded_String("Tost")
        );

   package Losowa_Konsumpcja is new
     Ada.Numerics.Discrete_Random(Zakres_Czasu_Konsumpcji);
   package Losowy_Zestaw is new
     Ada.Numerics.Discrete_Random(Typ_Zestawow);

   type My_Str is new String(1 ..256);

   -- Producent produkuje określony wyrób
   task type Producent is
      -- Nadaj Producentowi tożsamość, czyli rodzaj wyrobu
      entry Zacznij(Wyrob: in Typ_Wyrobow; Czas_Produkcji: in Integer);
      entry Pobierz(Ilosc: in Integer);
   end Producent;

   -- Konsument pobiera z Bufora dowolny zestaw składający się z wyrobów
   task type Konsument is
      -- Nadaj Konsumentowi tożsamość
      entry Zacznij(Numer_Konsumenta: in Typ_Konsumenta;
                    Czas_Konsumpcji: in Integer);
      entry Otrzymaj(Nr_Zestawu: in Integer);
   end Konsument;

   -- W Buforze następuje składanie wyrobów w zestawy
   task type Bufor is
      -- Przyjmij wyrób do magazynu, o ile jest miejsce
      entry Przyjmij(Wyrob: in Typ_Wyrobow; Ilosc: in Integer);
      -- Wydaj zestaw z magazynu, o ile są części (wyroby)
      entry Wydaj(Zestaw: in Typ_Zestawow; Klient: in Typ_Konsumenta);
      entry Zrezygnuj(Klient: in Typ_Konsumenta);
   end Bufor;

   P: array ( 1 .. Liczba_Wyrobow ) of Producent;
   K: array ( 1 .. Liczba_Konsumentow ) of Konsument;
   B: Bufor;

   task body Producent is
      package Losowa_Produkcja is new
	Ada.Numerics.Discrete_Random(Zakres_Czasu_Produkcji);
      G: Losowa_Produkcja.Generator;	--  generator liczb losowych
      Nr_Typu_Wyrobu: Integer;
      Numer_Wyrobu: Integer;
      Zaplecze: Integer;
      Produkcja: Integer;
   begin
      accept Zacznij(Wyrob: in Typ_Wyrobow; Czas_Produkcji: in Integer) do
	 Losowa_Produkcja.Reset(G);	--  zacznij generator liczb losowych
         Numer_Wyrobu := 1;
         Zaplecze := 0;
	 Nr_Typu_Wyrobu := Wyrob;
	 Produkcja := Czas_Produkcji;
      end Zacznij;
      Put_Line("Zaczęto producenta wyrobu " & Nazwa_Wyrobu(Nr_Typu_Wyrobu));
      loop
         delay Duration(Losowa_Produkcja.Random(G)); --  symuluj produkcję
         Zaplecze := Zaplecze + 1;
	 Put_Line("Wyprodukowano wyrób " & Nazwa_Wyrobu(Nr_Typu_Wyrobu)
		    & " numer "  & Integer'Image(Numer_Wyrobu));
	 -- Wstaw do magazynu
         B.Przyjmij(Nr_Typu_Wyrobu, Zaplecze);

         select
            accept Pobierz(Ilosc: in Integer) do
               Put_Line("Sprzedano wyrob " & Nazwa_Wyrobu(Nr_Typu_Wyrobu)
                          & " sztuk " & Integer'Image(Ilosc));
               Zaplecze := Zaplecze - Ilosc;
            end Pobierz;
         or delay Duration(Produkcja * 3);
            Put_Line("Timeout! Wznawianie produkcji " & Nazwa_Wyrobu(Nr_Typu_Wyrobu));
         end select;

	 Numer_Wyrobu := Numer_Wyrobu + 1;
      end loop;
   end Producent;

   task body Konsument is
      G: Losowa_Konsumpcja.Generator;	--  generator liczb losowych (czas)
      G2: Losowy_Zestaw.Generator;	--  też (zestawy)
      Nr_Konsumenta: Typ_Konsumenta;
      --Numer_Zestawu: Integer;
      Konsumpcja: Integer;
      Rodzaj_Zestawu: Integer;
      Nazwa_Konsumenta: constant array (1 .. Liczba_Konsumentow)
	of String(1 .. 10)
	:= ("Konsument1", "Konsument2");
   begin
      accept Zacznij(Numer_Konsumenta: in Typ_Konsumenta;
		     Czas_Konsumpcji: in Integer) do
	 Losowa_Konsumpcja.Reset(G);	--  ustaw generator
	 Losowy_Zestaw.Reset(G2);	--  też
	 Nr_Konsumenta := Numer_Konsumenta;
	 Konsumpcja := Czas_Konsumpcji;
      end Zacznij;
      Put_Line("Zaczęto konsumenta " & Nazwa_Konsumenta(Nr_Konsumenta));
      loop
	 delay Duration(Losowa_Konsumpcja.Random(G)); --  symuluj konsumpcję
	 Rodzaj_Zestawu := Losowy_Zestaw.Random(G2);
	 -- pobierz zestaw do konsumpcji
         B.Wydaj(Rodzaj_Zestawu, Nr_Konsumenta);

         Put_Line(Nazwa_Konsumenta(Nr_Konsumenta) & ": oczekiwanie na zestaw " &
		    Nazwa_Zestawu(Rodzaj_Zestawu));
         select
            accept Otrzymaj(Nr_Zestawu: in Integer) do
               Put_Line(Nazwa_Konsumenta(Nr_Konsumenta) & ": pobrano zestaw " &
		    Nazwa_Zestawu(Rodzaj_Zestawu) & " numer " &
                          Integer'Image(Nr_Zestawu));
            end Otrzymaj;
            or delay Duration(Konsumpcja * 3);
               Put_Line(Nazwa_Konsumenta(Nr_Konsumenta) & ": Timeout! " &
                          Nazwa_Zestawu(Rodzaj_Zestawu));
               B.Zrezygnuj(Nr_Konsumenta);
         end select;

      end loop;
   end Konsument;

   task body Bufor is
      Pojemnosc_Magazynu: constant Integer := 30;
      type Typ_Magazynu is array (Typ_Wyrobow) of Integer;
      Magazyn: Typ_Magazynu
	:= (0, 0, 0, 0, 0);
      Sklad_Zestawu: array(Typ_Zestawow, Typ_Wyrobow) of Integer
	:= ((1, 1, 0, 0, 1),
	    (1, 0, 1, 1, 1),
	    (1, 1, 0, 1, 0));
      Max_Sklad_Zestawu: array(Typ_Wyrobow) of Integer;
      Numer_Zestawu: array(Typ_Zestawow) of Integer
	:= (0, 0, 0);
      W_Magazynie: Integer := 0;

      type Array_Type is array (Positive range <>) of Integer;

      Q_P: Array_Type(Typ_Wyrobow)
        := (0,0,0,0,0);
      Q_P_ilosc : Array_Type(Typ_Wyrobow)
        := (0,0,0,0,0);
      Q_K: Array_Type(Typ_Konsumenta)
        := (0,0);
      Q_K_Zestawy: Array_Type(Typ_Konsumenta)
        := (0,0);

      Cierpliwosc_magazynu : Integer := 15;

      procedure Ustaw_Zmienne is
      begin
	 for W in Typ_Wyrobow loop
	    Max_Sklad_Zestawu(W) := 0;
	    for Z in Typ_Zestawow loop
	       if Sklad_Zestawu(Z, W) > Max_Sklad_Zestawu(W) then
		  Max_Sklad_Zestawu(W) := Sklad_Zestawu(Z, W);
	       end if;
	    end loop;
	 end loop;
      end Ustaw_Zmienne;

      function Mozna_Przyjac(Wyrob: Typ_Wyrobow) return Boolean is
	 Wolne: Integer;		--  wolne miejsce w magazynie
	 -- ile brakuje wyrobów w magazynie do produkcji dowolnego zestawu
	 Brak: array(Typ_Wyrobow) of Integer;
	 -- ile potrzeba miejsca w magazynie, by wyprodukować dowolny wyrób
	 Braki: Integer;
	 MP: Boolean;			--  można przyjąć
      begin
	 if W_Magazynie >= Pojemnosc_Magazynu then
	    return False;
	 end if;
	 -- W magazynie są wolne miejsca
	 Wolne := Pojemnosc_Magazynu - W_Magazynie;
	 MP := True;
	 for W in Typ_Wyrobow loop
	    if Magazyn(W) < Max_Sklad_Zestawu(W) then
	       MP := False;
	    end if;
	 end loop;
	 if MP then
	    return True;		--  w magazynie są już części na dowolny
	       				--  zestaw
	 end if;
	 if Integer'Max(0, Max_Sklad_Zestawu(Wyrob) - Magazyn(Wyrob)) > 0 then
	    -- właśnie tego wyrobu brakuje
	    return True;
	 end if;
	 Braki := 1;			--  dodajemy bieżący wyrób
	 for W in Typ_Wyrobow loop
	    Brak(W) := Integer'Max(0, Max_Sklad_Zestawu(W) - Magazyn(W));
	    Braki := Braki + Brak(W);
	 end loop;
	 if Wolne >= Braki then
	    -- w magazynie jest miejsce, żeby skompletować dowolny wyrób
	    return True;
	 else
	    -- brakuje miejsca dla takiej części
	    return False;
	 end if;
      end Mozna_Przyjac;

      function Mozna_Wydac(Zestaw: Typ_Zestawow) return Boolean is
      begin
	 for W in Typ_Wyrobow loop
	    if Magazyn(W) < Sklad_Zestawu(Zestaw, W) then
	       return False;
	    end if;
	 end loop;
	 return True;
      end Mozna_Wydac;

      procedure Porzadkuj_Kolejke(Kolejka: in out Array_Type) is
      begin
         for I in 1 .. Kolejka'Last loop
            if Kolejka(I) /= 0 then
               for J in 1 .. I-1 loop
                  if Kolejka(J) = 0 then
                    Kolejka(J) := Kolejka(I);
                    Kolejka(I) := 0;
                  end if;
               end loop;
            end if;
         end loop;
      end Porzadkuj_Kolejke;

      procedure Kolejka_Wstaw(Kolejka: in out Array_Type; Wartosc: in Integer) is
         Nr: Integer := 0;
      begin
         for I in 1 .. Kolejka'Last loop
            if Kolejka(I) = 0 then
               Nr := I;
               exit;
            end if;
         end loop;
         Kolejka(Nr) := Wartosc;
      end Kolejka_Wstaw;

      procedure Pisz_Kolejke(Kolejka: in out Array_Type) is
      begin
         for I in 1 .. Kolejka'Last loop
            Put_Line("LISTA " & Integer'Image(I) & " : " & Integer'Image(Kolejka(I)));
         end loop;
      end Pisz_Kolejke;

      procedure Sklad_Magazynu is
      begin
	 for W in Typ_Wyrobow loop
	    Put_Line("Skład magazynu: " & Integer'Image(Magazyn(W)) & " "
		       & Nazwa_Wyrobu(W));
	 end loop;
      end Sklad_Magazynu;

      procedure Kup is
         Ilosc: Integer := 0;
      begin
         for I in 1 .. Q_P'Last loop
            Ilosc := 0;
            if Q_P(I) /= 0 then
               while Mozna_Przyjac(Q_P(I)) and Ilosc < Q_P_ilosc(I) loop
                  Ilosc := Ilosc + 1;
                  Magazyn(Q_P(I)) := Magazyn(Q_P(I)) + 1;
                  W_Magazynie := W_Magazynie + 1;
               end loop;

               if Ilosc /= 0 then
                  P(Q_P(I)).Pobierz(Ilosc);
                  Sklad_Magazynu;
                  Q_P(I) := 0;
                  Q_P_ilosc(I) := 0;
               else
                  Put_Line("Nie mozna przyjac wyrobu " & Nazwa_Wyrobu(Q_P(I)));
               end if;
            end if;

         end loop;

         Porzadkuj_Kolejke(Q_P);
         Porzadkuj_Kolejke(Q_P_ilosc);
      end Kup;

      procedure Sprzedaj is
      begin
         for I in 1 .. Q_K'Last loop
            if Q_K(I) = 0 then
               exit;
            end if;
            if Mozna_Wydac(Q_K_Zestawy(I)) then
               for W in Typ_Wyrobow loop
                  Magazyn(W) := Magazyn(W) - Sklad_Zestawu(Q_K_Zestawy(I), W);
                  W_Magazynie := W_Magazynie - Sklad_Zestawu(Q_K_Zestawy(I), W);
               end loop;
               Numer_Zestawu(Q_K_Zestawy(I)) := Numer_Zestawu(Q_K_Zestawy(I)) + 1;

               K(I).Otrzymaj(Numer_Zestawu(Q_K_Zestawy(I)));
               Q_K(I) := 0;
               Q_K_Zestawy(I) := 0;
            else
               Put_Line("Brak części dla zestawu " & Nazwa_Zestawu(Q_K_Zestawy(I)));
            end if;
         end loop;

         Porzadkuj_Kolejke(Q_K);
         Porzadkuj_Kolejke(Q_K_Zestawy);
      end Sprzedaj;

   begin
      Put_Line("Zaczęto Bufor");
      Ustaw_Zmienne;
      loop
         select
            accept Przyjmij(Wyrob: in Typ_Wyrobow; Ilosc: in Integer) do
               Kolejka_Wstaw(Q_P, Wyrob);
               Kolejka_Wstaw(Q_P_ilosc, ilosc);
            end Przyjmij;
         or delay Duration(Cierpliwosc_magazynu);
              Put_Line("Brak potencjalnych sprzedawcow...");
         end select;
         Kup;
         --Pisz_Kolejke(Q_P);
         --Pisz_Kolejke(Q_P_ilosc);
         select
            accept Wydaj(Zestaw: in Typ_Zestawow; Klient: in Typ_Konsumenta) do
               Kolejka_Wstaw(Q_K, Klient);
               Kolejka_Wstaw(Q_K_Zestawy, Zestaw);
            end Wydaj;
         or delay Duration(Cierpliwosc_magazynu);
              Put_Line("Brak potencjalnych nabywcow...");
         end select;
         Sprzedaj;
         --Pisz_Kolejke(Q_K);
         --Pisz_Kolejke(Q_K_Zestawy);
         select
            accept Zrezygnuj (Klient : in Typ_Konsumenta) do
               for I in 1 .. Q_K'Last loop
                  if Q_K(I) = Klient then
                     Q_K(I) := 0;
                     Q_K_Zestawy(I) := 0;
                     exit;
                  end if;
               end loop;
               Porzadkuj_Kolejke(Q_K);
               Porzadkuj_Kolejke(Q_K_Zestawy);
            end Zrezygnuj;
         else
            null;
         end select;

      end loop;
   end Bufor;

begin
   for I in 1 .. P'Last loop
      P(I).Zacznij(I, 10);
   end loop;
   for I in 1 .. K'Last loop
      K(I).Zacznij(I, 12);
   end loop;
end Symulacja;


