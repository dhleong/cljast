
package net.dhleong.test;

public class Magic {

    static final int MAGIC_NUMBER = 42;

    // intentionally an object so we can call methods on it
    public Integer doMagic() {
        return Integer.valueOf(MAGIC_NUMBER);
    }

    public Magic get() {
        return this;
    }

    public void boring() {
    }

    public static Magic newInstance() {
        return new Magic();
    }
}
