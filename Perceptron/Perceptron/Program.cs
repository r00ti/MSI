﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Perceptron
{
    class Program
    {
        static void Main(string[] args)
        {
            var perc = new Neuron();

            perc.Uruchom();
            Console.ReadKey();
        }
    }
}
