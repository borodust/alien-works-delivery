package org.borodust.alienworks.android.demo;

import com.lispworks.LispCalls;
import com.lispworks.Manager;

import org.borodust.alienworks.android.AlienWorks;
import org.borodust.alienworks.android.AlienWorksActivity;


public class AlienWorksAppActivity extends AlienWorksActivity {
    static {
        System.loadLibrary("LispWorks");
        AlienWorks.loadLibrariesGlobally("libSDL2.so");
    }

    @Override
    protected void initializeAlienWorksApplication() {
        Manager.init(this, () ->
                AlienWorks.create().run(() -> LispCalls.callVoidV("alien-works::run")));
    }
}
