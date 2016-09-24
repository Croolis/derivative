#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <vector>
#include <iostream>

using namespace std;

int n = 100000000;
vector<int> prime;
vector<int> lowest_divisor(n + 1, 0);
vector<int> derivative(n + 1, 0);

int der_fd;
int div_fd;

int derivate(int x) {
	return derivative[x];
}

void setDerivative(int x, int value) {
	derivative[x] = value;
}

int getLowestDivisor(int x) {
	return lowest_divisor[x];
}

void setLowestDivisor(int x, int value) {
	lowest_divisor[x] = value;
}

int main() {
	for (int i = 2; i <= n; i++) {
		if (getLowestDivisor(i) == 0) {
			prime.push_back(i);
			setLowestDivisor(i, 0);
			setDerivative(i, 1);
		}
		for (int j = 0; j < prime.size(); j++) {
			int a = prime[j];
			int t = i * a;
			if (t > n)
				break;
			int b = i;
			
			int der = derivate(a) * b + a * derivate(b);
			setLowestDivisor(t, prime[j]);
			setDerivative(t, der);
		}
	}
}