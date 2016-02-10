#written with Ryan Jelks on 2/08/16

require 'rspec'

def calc_price(books)
  price = 0
  until books.empty? do
    unique_book_set = books.uniq
    discount_multiplier = calc_discount_multiplier(unique_book_set.length)
    price += unique_book_set.length * 8 * discount_multiplier
    unique_book_set.each do |book|
      books.slice!(books.find_index(book))
    end
  end
  price
end

def calc_discount_multiplier(num_books)
  case num_books
    when 2
      0.95
    when 3
      0.90
    when 4
      0.80
    when 5
      0.75
    else
      1
  end
end

describe '#calc_price' do
  it 'should calculate prices' do
    expect(calc_price []).to eq 0
    expect(calc_price [1]).to eq 8
    expect(calc_price [1, 2]).to eq 15.2
    expect(calc_price [1, 2, 3]).to eq 21.6
    expect(calc_price [1, 2, 3, 4]).to eq 25.6
    expect(calc_price [1, 2, 3, 4, 5]).to eq 30
    expect(calc_price [1, 2, 3, 3]).to eq 29.6
    expect(calc_price [1, 1, 2, 2, 3, 3, 4, 5]).to eq 51.6
  end
end

#not working yet.
describe '#best_sets' do
  it 'should return the optimal sets of books' do
    expect(best_sets([1,1,2,2,3,3,4,5])).to eq([[1,2,3,4][1,2,3,5]])
  end
end

#not working yet.
def best_sets book_arr
  unique_books = book_arr.uniq
  set_of_sets = []
  set = []
  unique_books.each do |book|
    set = []
    set << book_arr.slice!(book_arr.find_index(book))
  end
  set_of_sets << set
end