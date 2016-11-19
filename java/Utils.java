package ghcvm.time;

import java.util.Date;
import java.util.TimeZone;
import java.util.Calendar;

public class Utils {
    public static int getTZOffset(long t) {
        return TimeZone.getDefault().getOffset(t * 1000) / 1000;
    }

    public static boolean isDST(long t) {
        return TimeZone.getDefault().inDaylightTime(new Date(t * 1000));
    }

    public static String getTZ() {
        return TimeZone.getDefault().getDisplayName(false, TimeZone.SHORT);
    }

    public static long getCurrentTimeInSeconds() {
        return Calendar.getInstance().getTimeInMillis() / 1000;
    }
}
