using System.Collections.Generic;
using System.Linq;

namespace HarryPotter
{
    public class ShoppingCartCalculator
    {
        public decimal Calculate(List<Book> shoppingCart)
        {
            // Generate list of all sets of unique books in cart
            var setsOfUniqueBooks = new List<List<Book>>();

            while (shoppingCart.Count > 0)
            {
                var uniqueSet = new List<Book>();
                for (int i = 1; i <= 5; i++)
                {
                    var book = shoppingCart.FirstOrDefault(s => s.id == i);
                    if (book != null)
                    {
                        uniqueSet.Add(book);
                        shoppingCart.Remove(book);
                    }
                }
                setsOfUniqueBooks.Add(uniqueSet);
            }

            // If there is a set of 5 unique books and a set of 3 unique books, replace them with 2 sets of 4 unique books
            while (setsOfUniqueBooks.Any(set => set.Count == 3) && setsOfUniqueBooks.Any(set => set.Count == 5))
            {
                var setOfFive = setsOfUniqueBooks.FirstOrDefault(set => set.Count == 5);
                var setOfThree = setsOfUniqueBooks.FirstOrDefault(set => set.Count == 3);

                if (setOfThree != null & setOfFive != null)
                {
                    var bookNotInSetOfThree = (from books in setOfThree
                        from books2 in setOfFive
                        where books.id != books2.id
                        select books2).First();

                    setOfFive.Remove(bookNotInSetOfThree);
                    setOfThree.Add(bookNotInSetOfThree);
                }
            }
            return setsOfUniqueBooks.Sum(set => CalculateDiscount(set));
        }

        private decimal CalculateDiscount(List<Book> bookList)
        {
            switch (bookList.Count)
            {
                case 2:
                    return bookList.Sum(book => book.Cost)*0.95M;
                case 3:
                    return bookList.Sum(book => book.Cost)*0.90M;
                case 4:
                    return bookList.Sum(book => book.Cost)*0.80M;
                case 5:
                    return bookList.Sum(book => book.Cost)*0.75M;
                default:
                    return bookList.Sum(book => book.Cost);
            }
        }
    }
}