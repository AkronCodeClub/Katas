using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HarryPotter
{
    public class Book
    {
        public int id { get; set; }
        public decimal Cost { get; set; }

        public Book(int id)
        {
            this.id = id;
            Cost = 8;
        }
    }
}
