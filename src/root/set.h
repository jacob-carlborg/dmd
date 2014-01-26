// Copyright (c) 2014 by Digital Mars
// All Rights Reserved
// written by Jacob Carlborg and Walter Bright
// http://www.digitalmars.com
// License for redistribution is by either the Artistic License
// in artistic.txt, or the GNU General Public License in gnu.txt.
// See the included readme.txt for details.

#include <assert.h>
#include <stddef.h>

template <typename E> class Set
{
    class Element
    {
    public:
        Element* next;
        E value;
    };

    Element** elements;
    Element* init[4];
    Element first;
    size_t size;
    size_t length_;

public:

    Set ()
    {
        elements = init;
        size = sizeof(init) / sizeof(init[0]);
        length_ = 0;
        init[0] = NULL;
        init[1] = NULL;
        init[2] = NULL;
        init[3] = NULL;
        assert(size == 4);
    }

    // Rehashes the receiver.
    void rehash ()
    {
        size_t newSize = this->size;

        if (newSize == 4)
            newSize = 32;
        else
            newSize *= 4;

        Element** newElements = new Element*[newSize];
        memset(newElements, 0, newSize * sizeof(Element*));

        for (size_t i = 0; i < this->size; i++)
        {
            Element* element = this->elements[i];

            while (element)
            {
                Element* next = element->next;
                size_t j = Set::hash((size_t) element->value) & (newSize - 1);
                element->next = newElements[j];
                newElements[j] = element;
                element = next;
            }
        }

        if (this->elements != this->init)
            delete[] this->elements;

        this->elements = newElements;
        this->size = newSize;
    }

    /**
     * Adds the given element to the receiver unless it already contains the value.
     *
     * Return true if the given element was added, otherwise false.
     */
    bool add (E value)
    {
        assert(this->size);

        size_t hash = Set::hash((size_t) value) & (this->size - 1);
        Element** elements = &this->elements[hash];
        Element* element;

        while ((element = *elements) != NULL)
        {
            if (element->value == value)
                return false;
            elements = &element->next;
        }

        size_t length = ++this->length_;
        element = (length != 1) ? new Element() : &this->first;
        element->next = NULL;
        element->value = value;
        *elements = element;

        if (length > this->size * 2)
            rehash();

        return true;
    }

    // Returns true if the receiver contains the given element, otherwise false.
    bool contains (E value) const
    {
        size_t hash = Set::hash((size_t) value) & (this->size - 1);
        Element* element = this->elements[hash];

        while (element)
        {
            if (element->value == value)
                return true;
            element = element->next;
        }

        return false;
    }

    // Returns true if the receiver does not contain any elements, otherwise false.
    bool isEmpty () const
    {
        return this->length_ == 0;
    }

    // Returns the number of elements in the receiver.
    size_t length () const
    {
        return this->length_;
    }

private:

    static size_t hash (size_t value)
    {
        value ^= (value >> 20) ^ (value >> 12);
        return value ^ (value >> 7) ^ (value >> 4);
    }
};

#if UNITTEST

void unittest_set ()
{
    Set<int> set;
    assert(set.length() == 0);
    assert(set.isEmpty());
    assert(!set.contains(3));

    assert(set.add(4));
    assert(set.contains(4));
    assert(!set.isEmpty());
    assert(set.length() == 1);

    assert(set.add(18));
    assert(!set.add(18));
    assert(set.contains(18));
    assert(!set.isEmpty());
    assert(set.length() == 2);

    Set<const char*> strings;

    assert(strings.add("foo"));
    assert(strings.contains("foo"));
    assert(strings.length() == 1);

    assert(strings.add("bar"));
    assert(strings.contains("bar"));
    assert(strings.length() == 2);

    const char* str = "this is a test string";
    assert(strings.add(str));
    assert(strings.contains(str));
    assert(strings.length() == 3);

    strings.rehash();
    assert(strings);
}

#endif