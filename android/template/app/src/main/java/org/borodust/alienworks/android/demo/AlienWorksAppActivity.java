package org.borodust.alienworks.android.demo;

import org.borodust.alienworks.android.AlienWorks;
import org.borodust.alienworks.android.AlienWorksActivity;


public class AlienWorksAppActivity extends AlienWorksActivity {
    @Override
    protected void initializeAlienWorksApplication() {
        AlienWorks.create().run(() -> System.out.println("YO"));
    }
}
