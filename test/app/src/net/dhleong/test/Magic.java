
package net.dhleong.test;

public class Magic {

    static final int MAGIC_NUMBER = 42;

    // intentionally an object so we can call methods on it
    public Integer doMagic() {
        return Integer.valueOf(MAGIC_NUMBER);
    }

    /** Get some magic */
    public Magic get() {
        return this;
    }

    /* Be boring */
    public void boring() {
    }

    /** 
     * Create Magic 
     * @return a new piece of Magic
     */
    public static Magic newInstance(final String name) {
        return new Magic();
    }
}
