/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.fxd.composer.lexer;

import com.sun.javafx.tools.fxd.container.scene.fxd.FXDSyntaxErrorException;
import org.netbeans.api.lexer.Token;
import org.netbeans.spi.lexer.TokenPropertyProvider;

/**
 *
 * @author avk
 */
public class SyntaxErrorPropertyProvider  implements TokenPropertyProvider<FXDTokenId>{

    private FXDSyntaxErrorException m_error;

    public SyntaxErrorPropertyProvider(FXDSyntaxErrorException error) {
        m_error = error;
    }

    public Object getValue(Token<FXDTokenId> token, Object key) {
        if (key == FXDSyntaxErrorException.class){
            return m_error;
        }
        return null;
    }

}
