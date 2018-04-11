using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Perceptron
{
    public class Neuron
    {
        double[] w = new double[8];
        double x1, x2, d, s, y, counter;
        double waga, count, error;
        Random rand = new Random();

        public void Uruchom()
        {
            Console.WriteLine("Podaj ilość par");
            count = Convert.ToDouble(Console.ReadLine());
            Console.WriteLine("Podaj tolerancje błędu");
            error = Convert.ToDouble(Console.ReadLine());

            Ustaw(1, -1, count);

            for (int q = 0; q < count; q++)
            {
                WczytajWejscia();
                WypiszWejscia();
                UstawWyjscie(w);
            }
        }

        public double LosujWagi(int max, int min)
        {
            min = (min * 100);
            max = (max * 100);
            return waga = 0.01 * (rand.Next(min, max));
        }

        public void WczytajWejscia()
        {
            Console.WriteLine("Podaj wartosc x1");
            x1 = Convert.ToDouble(Console.ReadLine());
            Console.WriteLine("Podaj wartosc x2");
            x2 = Convert.ToDouble(Console.ReadLine());
            Console.WriteLine("Podaj wartosc d");
            d = Convert.ToDouble(Console.ReadLine());
            for (int i = 0; i < 3; i++)
            {
                Console.WriteLine($"w[{i}] = {w[i]}");
            }
        }
        public void WypiszWejscia()
        {
            Console.WriteLine($"x1 = {x1}, x2 = {x2} , d= {d}");
        }
        public void Ustaw(int wagaMax, int wagaMin, double ilosc)
        {
            for (int i = 0; i < 3; i++)
            {
                w[i] = LosujWagi(wagaMax, wagaMin);
            }
        }
        public double ObliczBlad(double wart_oczekiwana, double WyjscieNeuronu)
        {
            double blad = 0;
            blad = wart_oczekiwana - WyjscieNeuronu;
            return blad;
        }

        public void UstawWyjscie(double[] w)
        {
            s = 0;
            s = w[0] + w[1] * x1 + x2 * w[2];
            if (s >= 1) y = 1;
            else y = -1;
            double blad = 0;
            counter++;
            blad = ObliczBlad(d, s);
            if (y != d)
            {
                w[0] = w[0] * blad;
                w[1] = w[1] + x1 * blad;
                w[2] = w[2] + x2 * blad;
            }

            Console.WriteLine($"d= {d}");
            Console.WriteLine($"s= {s}");
            Console.WriteLine($"y= {y}");
            Console.WriteLine($"blad= {blad}");
            Console.WriteLine($"error= {error}");
            if (y == d) Console.WriteLine("Równe");
            if (count == counter)
            {
                Console.WriteLine("KONIEC EPOKI");
                if (blad > error)
                {
                    counter = 0;
                    Uruchom();
                }
            }
        }
    }
}
