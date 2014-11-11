// PLATFORM: osx
// REQUIRED_ARGS: -L-framework -LCocoa

extern (Objective-C)
class NSObject {
    static NSObject alloc() @selector("alloc");
    this() @selector("init");
}

void main() {
    auto o1 = new NSObject;
    auto o2 = new NSObject;
    synchronized (o1, o2) {
        o2 = null;
        o1 = null;
    }
}