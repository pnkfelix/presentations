#include <iostream>
#include <vector>

int gather_negs(std::vector<int> &input, std::vector<int> &out) { // C++
    int count = 0;
    for (auto it = input.begin(); it != input.end(); ++it) {
        auto val = *it;
        if (val < 0) { out.push_back(-val); count++; }
    }
    return count;
}

int main(int argc, char** argv)
{
    int values[] = { -1, 2, 3, -4, -5, -6, -7, -8, -9, 10};
    std::vector<int> input(values, values + sizeof(values) / sizeof(int));
    std::vector<int> myvector;

    gather_negs(input, myvector);

    std::cout << "myvector contains:";
    for (auto it = myvector.begin() ; it != myvector.end(); ++it)
        std::cout << ' ' << *it;
    std::cout << '\n';

    myvector = input;
    gather_negs(myvector, myvector);

    std::cout << "myvector contains:";
    for (auto it = myvector.begin() ; it != myvector.end(); ++it)
        std::cout << ' ' << *it;
    std::cout << '\n';

    return 0;
}

