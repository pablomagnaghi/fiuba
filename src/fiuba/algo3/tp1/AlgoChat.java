package fiuba.algo3.tp1;
import java.util.List;

public class AlgoChat {

	private Usuario usuario;
	private ChatsIndividuales chatsIndividuales;
	private ChatsGrupales chatsGrupales;
	
	
	public AlgoChat(String nombreUsuario) {
		usuario = new Usuario(nombreUsuario);
		chatsIndividuales = new ChatsIndividuales();
		chatsGrupales = new ChatsGrupales();
	}

	public int cantidadDeChatsIndividuales() {
		return chatsIndividuales.cantidadDeChats();
	}

	public int cantidadDeContactos() {
		return chatsIndividuales.cantidadDeContactos();
	}
	
	public int cantidadDeChatsGrupales() {
		return chatsGrupales.cantidadDeChats();
	}

	public int cantidadDeGrupos() {
		return chatsGrupales.cantidadDeGrupos();
	}

	public int cantidadDeMensajesEnviados() {
		return chatsIndividuales.cantidadDeMensajesEnviados() + chatsGrupales.cantidadDeMensajesEnviados();
	}

	public String nombreUsuario() {
		return usuario.getNombre();
	}

	public void agregarContacto(String nombreContacto) {
		chatsIndividuales.agregarContacto(nombreContacto);
	}

	public boolean existeContacto(String nombreContacto) {
		return chatsIndividuales.existeChat(nombreContacto);
	}

	public void recibirMensajeDe(String nombreContacto, String mensaje) {
		chatsIndividuales.recibirMensajeDe(nombreContacto, mensaje);
	}

	public int cantidadTotalMensajesRecibidos() {
		return chatsIndividuales.cantidadTotalMensajesRecibidos() + chatsGrupales.cantidadTotalMensajesRecibidos();
	}

	public int cantidadMensajesDe(String nombreContacto) {
		return chatsIndividuales.cantidadMensajesDe(nombreContacto);
	}

	public void enviarMensajeA(String nombreChat, String mensaje) {
		chatsIndividuales.enviarMensajeA(nombreChat, mensaje);
	}

	public int cantidadTotalMensajesEnviados() {
		return chatsIndividuales.cantidadTotalMensajesEnviados() + chatsGrupales.cantidadTotalMensajesEnviados();
	}

	public int cantidadMensajesEnviadosA(String nombreContacto) {
		return chatsIndividuales.cantidadMensajesEnviadosA(nombreContacto);
	}

	public List<String> obtenerConversacionCon(String nombreChat) {
		return chatsIndividuales.obtenerConversacionCon(nombreChat);
	}

	public void crearGrupo(String nombreGrupo) {
		chatsGrupales.crearGrupo(nombreGrupo);	
	}

	public void agregarContactoAGrupo(String nombreGrupo, String nombreContacto) {
		chatsGrupales.agregarContactoAGrupo(nombreGrupo, nombreContacto);
	}

	public int cantidadMiembrosEnGrupo(String nombreGrupo) {
		return chatsGrupales.cantidadMiembrosEnGrupo(nombreGrupo);
	}

	public boolean existeGrupo(String nombreGrupo) {
		return chatsGrupales.existeChat(nombreGrupo);
	}

	public void recibirMensajeDeGrupo(String nombreGrupo, String nombreContacto, String mensaje) {
		chatsGrupales.recibirMensajeDeGrupo(nombreGrupo, nombreContacto, mensaje);
	}

	public void enviarMensajeAGrupo(String nombreChat, String mensaje) {
		chatsGrupales.enviarMensajeA(nombreChat, mensaje);
	}

	public int cantidadMensajesRecibidosDelGrupo(String nombreGrupo) {
		return chatsGrupales.cantidadMensajesRecibidosDelGrupo(nombreGrupo);
	}

	public int cantidadMensajesEnviadosAlGrupo(String nombreGrupo) {
		return chatsGrupales.cantidadMensajesEnviadosAlGrupo(nombreGrupo);
	}
	
	public List<String> obtenerConversacionConGrupo(String nombreChat) {
		return chatsGrupales.obtenerConversacionCon(nombreChat);
	}

	public void borrarMensajesDelContacto(String nombreChat) {
		chatsIndividuales.borrarMensajes(nombreChat);
	}
	
	public void borrarMensajesDelGrupo(String nombreChat) {
		chatsGrupales.borrarMensajes(nombreChat);
	}

	


}
